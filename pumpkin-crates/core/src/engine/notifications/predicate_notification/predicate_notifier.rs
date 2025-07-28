use super::predicate_tracker_for_domain::PredicateTrackerForDomain;
use super::PredicateIdAssignments;
use super::PredicateValue;
use crate::basic_types::PredicateId;
use crate::basic_types::PredicateIdGenerator;
use crate::containers::KeyedVec;
use crate::containers::StorageKey;
use crate::engine::notifications::DomainEvent;
use crate::engine::Assignments;
use crate::engine::TrailedValues;
use crate::predicates::Predicate;
use crate::predicates::PredicateType;
use crate::variables::DomainId;

/// An orchestrating struct which serves as the main contact point for the solver with
/// [`PredicateNotifier`].
///
/// Contains method for creating [`PredicateId`]s for [`Predicate`]s, retrieving the list of
/// updated [`Predicate`]s, and allowing propagators to indicate that the polarity of a
/// [`Predicate`] should be tracked (i.e. adding a [`Predicate`] to the scope of
/// [`PredicateNotifier`]).
///
/// It also contains the [`PredicateIdAssignments`] which serves as a (lazy) structure for
/// retrieving the polarity of [`Predicate`]s (represented by [`PredicateId`]s).
#[derive(Default, Debug)]
pub(crate) struct PredicateNotifier {
    /// Maps a [`Predicate`] to a [`PredicateId`]
    pub(crate) predicate_to_id: PredicateIdGenerator,
    /// Tracks the current status for [`PredicateId`]s.
    pub(crate) predicate_id_assignments: PredicateIdAssignments,
    /// Contains the [`PredicateTrackerForDomain`] for each [`DomainId`]
    domain_id_to_predicate_tracker: KeyedVec<DomainId, PredicateTrackerForDomain>,
}

impl PredicateNotifier {
    pub(crate) fn debug_empty_clone(&self) -> Self {
        let predicate_id_assignments = self.predicate_id_assignments.debug_empty_clone();
        Self {
            predicate_to_id: self.predicate_to_id.clone(),
            predicate_id_assignments,
            ..Default::default()
        }
    }

    pub(crate) fn debug_create_from_assignments(&mut self, assignments: &Assignments) {
        self.predicate_id_assignments
            .debug_create_from_assignments(assignments, &mut self.predicate_to_id);
    }

    /// Returns the falsified predicates; note that this structure will be cleared once it is
    /// dropped.
    pub(crate) fn drain_falsified_predicates(&mut self) -> impl Iterator<Item = PredicateId> + '_ {
        self.predicate_id_assignments.drain_falsified_predicates()
    }

    /// Returns the satisfied predicates; note that this structure will be cleared once it is
    /// dropped.
    pub(crate) fn drain_satisfied_predicates(&mut self) -> impl Iterator<Item = PredicateId> + '_ {
        self.predicate_id_assignments.drain_satisfied_predicates()
    }

    /// Returns the [`Predicate`] corresponding to a [`PredicateId`].
    ///
    /// This method will panic if there is no [`Predicate`] for the provided [`PredicateId`].
    pub(crate) fn get_predicate(&self, predicate_id: PredicateId) -> Predicate {
        self.predicate_to_id.get_predicate(predicate_id)
    }

    /// Method which is called when an update to a [`DomainId`] has taken place
    ///
    /// This method will pass it along to the correct [`PredicateNotifier::on_update`]
    /// corresponding to the [`DomainId`] for which the update took place.
    pub(crate) fn on_update(
        &mut self,
        trailed_values: &mut TrailedValues,
        assignments: &Assignments,
        event: DomainEvent,
        domain: DomainId,
    ) {
        match event {
            DomainEvent::Assign => {
                self.on_update_predicate(
                    domain,
                    PredicateType::Equal,
                    assignments,
                    trailed_values,
                    None,
                );
            }
            DomainEvent::LowerBound => {
                self.on_update_predicate(
                    domain,
                    PredicateType::LowerBound,
                    assignments,
                    trailed_values,
                    None,
                );
            }
            DomainEvent::UpperBound => {
                self.on_update_predicate(
                    domain,
                    PredicateType::UpperBound,
                    assignments,
                    trailed_values,
                    None,
                );
            }
            DomainEvent::Removal => assignments
                .get_holes_on_current_decision_level(domain)
                .for_each(|value| {
                    self.on_update_predicate(
                        domain,
                        PredicateType::NotEqual,
                        assignments,
                        trailed_values,
                        Some(value),
                    );
                }),
        }
    }

    /// Method which is called when an update to a [`DomainId`] has taken place (provided in the
    /// form of a [Predicate]).
    ///
    /// This method will pass it along to the correct [`PredicateNotifier::on_update`]
    /// corresponding to the [`DomainId`] for which the update took place.
    fn on_update_predicate(
        &mut self,
        domain: DomainId,
        predicate_type: PredicateType,
        assignments: &Assignments,
        trailed_values: &mut TrailedValues,
        removed_value: Option<i32>,
    ) {
        if self.domain_id_to_predicate_tracker.len() <= domain.index() {
            // If no predicate has been registered for this domain id then we do nothing
            return;
        }

        // Otherwise we update the structures
        self.domain_id_to_predicate_tracker[domain].on_update(
            domain,
            predicate_type,
            assignments,
            trailed_values,
            &mut self.predicate_id_assignments,
            removed_value,
        );
    }

    /// This method will extend the scope of [`PredicateNotifier`] by adding the provided
    /// [`Predicate`] to its scope.
    pub(crate) fn track_predicate(
        &mut self,
        id: PredicateId,
        trailed_values: &mut TrailedValues,
        assignments: &Assignments,
    ) {
        let predicate = self.predicate_to_id.get_predicate(id);

        // First, we resize the number of DomainIds for which we store predicate trackers
        if self.domain_id_to_predicate_tracker.len() <= predicate.get_domain().index() {
            self.domain_id_to_predicate_tracker.resize(
                predicate.get_domain().index() + 1,
                PredicateTrackerForDomain::new(),
            );
        }

        // Now we initialise the predicate tracker; this does not add it to the scope yet but it
        // initialises the structures
        self.domain_id_to_predicate_tracker[predicate.get_domain()].initialise(
            predicate,
            assignments.get_initial_lower_bound(predicate.get_domain()),
            assignments.get_initial_upper_bound(predicate.get_domain()),
            trailed_values,
        );

        // Now we add it to the scope of the tracker
        //
        // We check whether it was already tracked or not
        let was_not_already_tracked = self.domain_id_to_predicate_tracker[predicate.get_domain()]
            .watch_predicate(predicate, id);

        // If it was not already tracked then we store the update; otherwise we assume that the
        // cache has already been informed of its value
        if was_not_already_tracked {
            // Then we cache the known value of the predicate; note that this method also ensures
            // that it is added to the list of PredicateIds which the propagators should
            // be notified about if it has not done so already
            self.predicate_id_assignments.store_predicate(
                id,
                match assignments.evaluate_predicate(predicate) {
                    Some(satisfied) => {
                        if satisfied {
                            PredicateValue::AssignedTrue
                        } else {
                            PredicateValue::AssignedFalse
                        }
                    }
                    None => PredicateValue::Unknown,
                },
            );
        }
    }
}
