use super::PredicateIdAssignments;
use crate::basic_types::PredicateId;
use crate::basic_types::PredicateIdGenerator;
use crate::containers::KeyedVec;
use crate::containers::StorageKey;
use crate::engine::Assignments;
use crate::engine::TrailedValues;
use crate::engine::notifications::DomainEvent;
use crate::engine::notifications::predicate_notification::predicate_tracker::PredicateTracker;
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
#[derive(Default, Debug, Clone)]
pub(crate) struct PredicateNotifier {
    /// Maps a [`Predicate`] to a [`PredicateId`]
    pub(crate) predicate_to_id: PredicateIdGenerator,
    /// Tracks the current status for [`PredicateId`]s.
    pub(crate) predicate_id_assignments: PredicateIdAssignments,
    /// Contains the [`PredicateTracker`] for each [`DomainId`]
    domain_id_to_predicate_tracker: KeyedVec<DomainId, PredicateTracker>,
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
                self.on_update_predicate(domain, PredicateType::Equal, assignments, trailed_values);
            }
            DomainEvent::LowerBound => {
                self.on_update_predicate(
                    domain,
                    PredicateType::LowerBound,
                    assignments,
                    trailed_values,
                );
            }
            DomainEvent::UpperBound => {
                self.on_update_predicate(
                    domain,
                    PredicateType::UpperBound,
                    assignments,
                    trailed_values,
                );
            }
            DomainEvent::Removal => {
                self.on_update_predicate(
                    domain,
                    PredicateType::NotEqual,
                    assignments,
                    trailed_values,
                );
            }
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
    ) {
        if self.domain_id_to_predicate_tracker.len() <= domain.index() {
            // If no predicate has been registered for this domain id then we do nothing
            return;
        }

        if predicate_type.is_disequality() {
            for removed_value in assignments.get_holes_at_current_checkpoint(domain) {
                let predicate =
                    predicate_type.into_predicate(domain, assignments, Some(removed_value));

                self.domain_id_to_predicate_tracker[domain].on_update(
                    predicate,
                    trailed_values,
                    &mut self.predicate_id_assignments,
                );
            }
        } else {
            // Otherwise we update the structures
            self.domain_id_to_predicate_tracker[domain].on_update(
                predicate_type.into_predicate(domain, assignments, None),
                trailed_values,
                &mut self.predicate_id_assignments,
            );
        }
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
            self.domain_id_to_predicate_tracker
                .resize(predicate.get_domain().index() + 1, PredicateTracker::new());
        }

        // Now we initialise the predicate tracker; this does not add it to the scope yet but it
        // initialises the structures
        self.domain_id_to_predicate_tracker[predicate.get_domain()].initialise(
            predicate.get_domain(),
            assignments.get_initial_lower_bound(predicate.get_domain()),
            assignments.get_initial_upper_bound(predicate.get_domain()),
            trailed_values,
        );

        // Now we add it to the scope of the tracker
        //
        // We check whether it was already tracked or not
        let _ = self.domain_id_to_predicate_tracker[predicate.get_domain()].track(predicate, id);
    }
}
