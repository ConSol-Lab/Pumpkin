use log::info;

use super::faithfulness_for_domain::DomainFaithfulnessForDomain;
use crate::basic_types::PredicateId;
use crate::basic_types::PredicateIdGenerator;
use crate::containers::KeyedVec;
use crate::containers::StorageKey;
use crate::engine::Assignments;
use crate::engine::TrailedValues;
use crate::predicates::Predicate;
use crate::variables::DomainId;

/// An orchestrating struct which serves as the main contact point for propagators with
/// [`DomainFaithfulness`].
///
/// Contains method for creating [`PredicateId`]s for [`Predicate`]s, retrieving the list of
/// updated [`Predicate`]s, and allowing propagators to indicate that the polarity of a
/// [`Predicate`] should be tracked (i.e. adding a [`Predicate`] to the scope of
/// [`DomainFaithfulness`]).
#[derive(Default, Debug)]
pub(crate) struct DomainFaithfulness {
    /// Maps a [`Predicate`] to a [`PredicateId`]
    pub(crate) predicate_to_id: PredicateIdGenerator,
    /// Contains the [`Faithfulness`] for each [`DomainId`]
    domain_id_to_faithfullness: KeyedVec<DomainId, DomainFaithfulnessForDomain>,
    /// A list of the predicates which have been found to be falsified since the last round of
    /// notifications
    falsified_predicates: Vec<PredicateId>,
    /// A list of the predicates which have been found to be satisfied since the last round of
    /// notifications
    pub(crate) satisfied_predicates: Vec<PredicateId>,
}

impl DomainFaithfulness {
    /// Returns the falsified predicates; note that this structure will be cleared once it is
    /// dropped.
    #[allow(dead_code, reason = "Will be part of the public API")]
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

    /// Method which is called when an update to a [`DomainId`] has taken place (provided in the
    /// form of a [Predicate]).
    ///
    /// This method will pass it along to the correct [`Faithfulness::on_update`]
    /// corresponding to the [`DomainId`] for which the update took place.
    pub(crate) fn on_update(
        &mut self,
        predicate: Predicate,
        trailed_values: &mut TrailedValues,
        assignments: &Assignments,
    ) {
        if self.domain_id_to_faithfullness.len() <= predicate.get_domain().index() {
            // If no predicate has been registered for this domain id then we do nothing
            return;
        }

        info!(
            "Faithfulness updated: {predicate} - {}, {}",
            assignments.get_lower_bound(predicate.get_domain()),
            assignments.get_upper_bound(predicate.get_domain())
        );

        // Otherwise we update the structures
        self.domain_id_to_faithfullness[predicate.get_domain()].on_update(
            predicate,
            trailed_values,
            &mut self.falsified_predicates,
            &mut self.satisfied_predicates,
            self.predicate_to_id
                .has_id_for_predicate(predicate)
                .then(|| self.predicate_to_id.get_id(predicate)),
        );
    }

    /// This method will extend the scope of [`DomainFaithfulness`] by adding the provided
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
            info!("Adding watcher for {predicate} with id {id:?}",);

            while self.domain_id_to_faithfullness.len() <= predicate.get_domain().index() {
                let _ = self
                    .domain_id_to_faithfullness
                    .push(DomainFaithfulnessForDomain::new(trailed_values));
            }

            self.domain_id_to_faithfullness[predicate.get_domain()].initialise(
                predicate.get_domain(),
                assignments.get_initial_lower_bound(predicate.get_domain()),
                assignments.get_initial_upper_bound(predicate.get_domain()),
            );

            // Then we update the structures
            self.domain_id_to_faithfullness[predicate.get_domain()].watch_predicate(
                predicate,
                id,
                trailed_values,
                assignments,
            );
        } else {
            info!("Adding existing watcher for {predicate} with id {id:?}")
        }

        id
    }
}
