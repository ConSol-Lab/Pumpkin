use crate::containers::SparseSet;
use crate::engine::Assignments;
use crate::predicates::Predicate;

/// Used to compute the LBD of nogoods. The type carries state that prevents the re-allocation of
/// helper data structures.
#[derive(Clone, Debug)]
pub(crate) struct Lbd {
    lbd_helper: SparseSet<u32>,
}

impl Default for Lbd {
    fn default() -> Self {
        fn sparse_set_mapping(elem: &u32) -> usize {
            *elem as usize
        }

        Lbd {
            lbd_helper: SparseSet::new(vec![], sparse_set_mapping),
        }
    }
}

impl Lbd {
    /// Compute the LBD of the given nogood under the given assignment.
    pub(crate) fn compute_lbd(
        &mut self,
        predicates: &[Predicate],
        assignments: &Assignments,
    ) -> u32 {
        self.lbd_helper.clear();
        self.lbd_helper
            .accommodate(&(assignments.get_decision_level() as u32));

        for predicate in predicates {
            let decision_level = assignments
                .get_decision_level_for_predicate(predicate)
                .unwrap();

            self.lbd_helper.insert(decision_level as u32);
        }

        self.lbd_helper.len() as u32
    }
}
