use super::predicate_tracker_for_domain::PredicateTrackerForDomain;
use super::PredicateIdAssignments;
use super::PredicateIdInfo;
use crate::basic_types::PredicateId;
use crate::basic_types::PredicateIdGenerator;
use crate::containers::KeyedVec;
use crate::containers::StorageKey;
use crate::engine::notifications::DomainEvent;
use crate::engine::Assignments;
use crate::engine::TrailedValues;
use crate::predicate;
use crate::predicates::Predicate;
use crate::variables::DomainId;

/// An orchestrating struct which serves as the main contact point for the solver with
/// [`PredicateNotifier`].
///
/// Contains method for creating [`PredicateId`]s for [`Predicate`]s, retrieving the list of
/// updated [`Predicate`]s, and allowing propagators to indicate that the polarity of a
/// [`Predicate`] should be tracked (i.e. adding a [`Predicate`] to the scope of
/// [`PredicateNotifier`]).
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
        let mut predicate_id_assignments = PredicateIdAssignments::default();
        for predicate_id in self.predicate_id_assignments.predicate_ids() {
            predicate_id_assignments.store_predicate(predicate_id, PredicateIdInfo::Untracked);
        }
        Self {
            predicate_to_id: self.predicate_to_id.clone(),
            predicate_id_assignments,
            ..Default::default()
        }
    }

    pub(crate) fn debug_create_from_assignments(&mut self, assignments: &Assignments) {
        self.predicate_id_assignments
            .predicate_ids()
            .collect::<Vec<_>>()
            .into_iter()
            .for_each(|predicate_id| {
                let predicate = self.predicate_to_id.get_predicate(predicate_id);

                let value = if self.predicate_id_assignments.is_untracked(predicate_id) {
                    PredicateIdInfo::Untracked
                } else {
                    match assignments.evaluate_predicate(predicate) {
                        Some(assigned) => {
                            if assigned {
                                PredicateIdInfo::AssignedTrue
                            } else {
                                PredicateIdInfo::AssignedFalse
                            }
                        }
                        None => PredicateIdInfo::Unassigned,
                    }
                };
                self.predicate_id_assignments
                    .store_predicate(predicate_id, value);
            });
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
                    predicate!(domain == assignments.get_assigned_value(&domain).unwrap()),
                    trailed_values,
                );
            }
            DomainEvent::LowerBound => {
                self.on_update_predicate(
                    predicate!(domain >= assignments.get_lower_bound(domain)),
                    trailed_values,
                );
            }
            DomainEvent::UpperBound => {
                self.on_update_predicate(
                    predicate!(domain <= assignments.get_upper_bound(domain)),
                    trailed_values,
                );
            }
            DomainEvent::Removal => assignments
                .get_holes_on_decision_level(domain, assignments.get_decision_level())
                .for_each(|value| {
                    self.on_update_predicate(predicate!(domain != value), trailed_values);
                }),
        }
    }

    /// Method which is called when an update to a [`DomainId`] has taken place (provided in the
    /// form of a [Predicate]).
    ///
    /// This method will pass it along to the correct [`PredicateNotifier::on_update`]
    /// corresponding to the [`DomainId`] for which the update took place.
    fn on_update_predicate(&mut self, predicate: Predicate, trailed_values: &mut TrailedValues) {
        if self.domain_id_to_predicate_tracker.len() <= predicate.get_domain().index() {
            // If no predicate has been registered for this domain id then we do nothing
            return;
        }

        // Otherwise we update the structures
        self.domain_id_to_predicate_tracker[predicate.get_domain()].on_update(
            predicate,
            trailed_values,
            &mut self.predicate_id_assignments,
            self.predicate_to_id
                .has_id_for_predicate(predicate)
                .then(|| self.predicate_to_id.get_id(predicate)),
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

        while self.domain_id_to_predicate_tracker.len() <= predicate.get_domain().index() {
            let _ = self
                .domain_id_to_predicate_tracker
                .push(PredicateTrackerForDomain::new(trailed_values));
        }

        self.predicate_id_assignments.store_predicate(
            id,
            match assignments.evaluate_predicate(predicate) {
                Some(satisfied) => {
                    if satisfied {
                        PredicateIdInfo::AssignedTrue
                    } else {
                        PredicateIdInfo::AssignedFalse
                    }
                }
                None => PredicateIdInfo::Unassigned,
            },
        );

        self.domain_id_to_predicate_tracker[predicate.get_domain()].initialise(
            predicate.get_domain(),
            assignments.get_initial_lower_bound(predicate.get_domain()),
            assignments.get_initial_upper_bound(predicate.get_domain()),
        );

        // Then we update the structures
        self.domain_id_to_predicate_tracker[predicate.get_domain()].watch_predicate(
            predicate,
            id,
            trailed_values,
            assignments,
        );
    }
}
