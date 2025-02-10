use log::info;

use super::faithfulness_list::Faithfullness;
use crate::basic_types::PredicateId;
use crate::basic_types::PredicateIdGenerator;
use crate::containers::KeyedVec;
use crate::containers::StorageKey;
use crate::engine::Assignments;
use crate::engine::TrailedAssignments;
use crate::predicates::Predicate;
use crate::pumpkin_assert_simple;
use crate::variables::DomainId;

#[derive(Default, Debug)]
pub(crate) struct DomainFaithfulness {
    pub(crate) predicate_to_id: PredicateIdGenerator,

    domain_id_to_faithfullness: KeyedVec<DomainId, Faithfullness>,

    falsified_predicates: Vec<PredicateId>,
    pub(crate) satisfied_predicates: Vec<PredicateId>,

    last_updated: usize,
}

impl DomainFaithfulness {
    pub(crate) fn backtrack_has_occurred(&mut self) {
        self.last_updated += 1;
    }

    #[allow(dead_code, reason = "Will be part of the public API")]
    pub(crate) fn drain_falsified_predicates(&mut self) -> impl Iterator<Item = PredicateId> + '_ {
        self.falsified_predicates.drain(..)
    }

    pub(crate) fn drain_satisfied_predicates(&mut self) -> impl Iterator<Item = PredicateId> + '_ {
        self.satisfied_predicates.drain(..)
    }

    pub(crate) fn get_predicate_for_id(&self, predicate_id: PredicateId) -> Predicate {
        self.predicate_to_id.get_predicate(predicate_id).unwrap()
    }

    pub(crate) fn get_id_for_predicate(&mut self, predicate: Predicate) -> Option<PredicateId> {
        if !self.predicate_to_id.has_id_for_predicate(predicate) {
            return None;
        }

        Some(self.predicate_to_id.get_id(predicate))
    }

    pub(crate) fn has_been_updated(
        &mut self,
        predicate: Predicate,
        stateful_assignments: &mut TrailedAssignments,
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
        self.domain_id_to_faithfullness[predicate.get_domain()].has_been_updated(
            predicate,
            stateful_assignments,
            &mut self.falsified_predicates,
            &mut self.satisfied_predicates,
            self.predicate_to_id
                .has_id_for_predicate(predicate)
                .then(|| self.predicate_to_id.get_id(predicate)),
        );
    }

    pub(crate) fn watch_predicate(
        &mut self,
        predicate: Predicate,
        stateful_assignments: &mut TrailedAssignments,
        assignments: &Assignments,
    ) -> PredicateId {
        pumpkin_assert_simple!(
            predicate.get_right_hand_side()
                <= assignments.get_initial_upper_bound(predicate.get_domain())
                || predicate.get_right_hand_side()
                    >= assignments.get_initial_lower_bound(predicate.get_domain()),
            "Attempted to create watcher for predicate {predicate:?}"
        );
        // If it is already watched then at the moment we do nothing
        let has_id_for_predicate = self.predicate_to_id.has_id_for_predicate(predicate);

        // We create a new predicate ID for the predicate
        let id = self.predicate_to_id.get_id(predicate);

        if !has_id_for_predicate {
            info!("Adding watcher for {predicate} with id {id:?}",);

            while self.domain_id_to_faithfullness.len() <= predicate.get_domain().index() {
                let _ = self
                    .domain_id_to_faithfullness
                    .push(Faithfullness::new(stateful_assignments));
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
                stateful_assignments,
                assignments,
            );
        } else {
            info!("Adding existing watcher for {predicate} with id {id:?}")
        }

        id
    }
}
