use crate::containers::SparseSet;
use crate::predicates::Predicate;
use crate::propagation::ReadDomains;

/// Used to compute the LBD of nogoods.
/// The type carries state that prevents the re-allocation of helper data structures.
#[derive(Clone, Debug)]
pub struct Lbd {
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
    pub fn compute_lbd<Context: ReadDomains>(
        &mut self,
        predicates: &[Predicate],
        context: &Context,
    ) -> u32 {
        self.lbd_helper.set_to_empty();
        self.lbd_helper
            .accommodate(&(context.get_checkpoint() as u32));

        for predicate in predicates {
            let checkpoint = context.get_checkpoint_for_predicate(*predicate).unwrap();

            self.lbd_helper.insert(checkpoint as u32);
        }

        self.lbd_helper.len() as u32
    }
}
