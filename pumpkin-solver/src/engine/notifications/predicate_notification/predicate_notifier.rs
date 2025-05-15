use super::predicate_tracker_for_domain::PredicateTrackerForDomain;
use crate::basic_types::PredicateId;
use crate::basic_types::PredicateIdGenerator;
use crate::containers::KeyedVec;
use crate::containers::StorageKey;
use crate::engine::notifications::domain_event_notification::DomainEvent;
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
    /// Contains the [`PredicateTrackerForDomain`] for each [`DomainId`]
    domain_id_to_predicate_tracker: KeyedVec<DomainId, PredicateTrackerForDomain>,
    /// A list of the predicates which have been found to be falsified since the last round of
    /// notifications
    falsified_predicates: Vec<PredicateId>,
    /// A list of the predicates which have been found to be satisfied since the last round of
    /// notifications
    pub(crate) satisfied_predicates: Vec<PredicateId>,
}

impl PredicateNotifier {
    /// Returns the falsified predicates; note that this structure will be cleared once it is
    /// dropped.
    pub(crate) fn drain_falsified_predicates(&mut self) -> impl Iterator<Item = PredicateId> + '_ {
        self.falsified_predicates.drain(..)
    }

    /// Returns the satisfied predicates; note that this structure will be cleared once it is
    /// dropped.
    pub(crate) fn drain_satisfied_predicates(&mut self) -> impl Iterator<Item = PredicateId> + '_ {
        self.satisfied_predicates.drain(..)
    }

    /// Returns the [`Predicate`] corresponding to a [`PredicateId`].
    ///
    /// This method will panic if there is no [`Predicate`] for the provided [`PredicateId`].
    pub(crate) fn get_predicate_for_id(&self, predicate_id: PredicateId) -> Predicate {
        self.predicate_to_id.get_predicate(predicate_id).unwrap()
    }

    /// Returns the [`PredicateId`] for the provided [`Predicate`].
    ///
    /// If there exists no [`PredicateId`] for the [`Predicate`] then this method will return
    /// [`None`].
    pub(crate) fn get_id_for_predicate(&mut self, predicate: Predicate) -> Option<PredicateId> {
        if !self.predicate_to_id.has_id_for_predicate(predicate) {
            return None;
        }

        Some(self.predicate_to_id.get_id(predicate))
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
            &mut self.falsified_predicates,
            &mut self.satisfied_predicates,
            self.predicate_to_id
                .has_id_for_predicate(predicate)
                .then(|| self.predicate_to_id.get_id(predicate)),
        );
    }

    /// This method will extend the scope of [`PredicateNotifier`] by adding the provided
    /// [`Predicate`] to its scope.
    pub(crate) fn track_predicate(
        &mut self,
        predicate: Predicate,
        trailed_values: &mut TrailedValues,
        assignments: &Assignments,
    ) -> PredicateId {
        // If it is already watched then at the moment we do nothing
        let has_id_for_predicate = self.predicate_to_id.has_id_for_predicate(predicate);

        // We create a new predicate ID for the predicate
        let id = self.predicate_to_id.get_id(predicate);

        if !has_id_for_predicate {
            while self.domain_id_to_predicate_tracker.len() <= predicate.get_domain().index() {
                let _ = self
                    .domain_id_to_predicate_tracker
                    .push(PredicateTrackerForDomain::new(trailed_values));
            }

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

        id
    }
}
