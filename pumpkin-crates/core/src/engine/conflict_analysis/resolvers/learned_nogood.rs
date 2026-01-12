use std::ops::Deref;

use crate::conflict_analysis::ConflictAnalysisContext;
use crate::predicates::Predicate;

/// A structure which stores a learned nogood
///
/// There are two assumptions:
/// - The asserting literal (i.e. the literal of the current decision level) is placed at the `0`th
///   index of [`LearnedNogood::literals`].
/// - A literal from the second-highest decision level is placed at the `1`st index of
///   [`LearnedNogood::literals`].
#[derive(Clone, Debug)]
pub struct LearnedNogood {
    pub predicates: Vec<Predicate>,
    pub backjump_level: usize,
}

impl Deref for LearnedNogood {
    type Target = [Predicate];

    fn deref(&self) -> &Self::Target {
        &self.predicates
    }
}

impl LearnedNogood {
    /// Creates a [`LearnedNogood`] from the provided [`Vec`].
    ///
    /// This method automatically ensures that the invariants of nogoods hold; see [`LearnedNogood`]
    /// for more details on these invariants.
    pub(crate) fn create_from_vec(
        mut clean_nogood: Vec<Predicate>,
        context: &ConflictAnalysisContext,
    ) -> Self {
        // We perform a linear scan to maintain the two invariants:
        // - The predicate from the current decision level is placed at index 0
        // - The predicate from the highest decision level below the current is placed at index 1
        let mut index = 1;
        let mut highest_level_below_current = 0;
        while index < clean_nogood.len() {
            let predicate = clean_nogood[index];
            let dl = context
                .state
                .get_checkpoint_for_predicate(predicate)
                .unwrap();

            if dl == context.state.get_checkpoint() {
                clean_nogood.swap(0, index);
                index -= 1;
            } else if dl > highest_level_below_current {
                highest_level_below_current = dl;
                clean_nogood.swap(1, index);
            }

            index += 1;
        }

        // The second highest decision level predicate is at position one.
        // This is the backjump level.
        let backjump_level = if clean_nogood.len() > 1 {
            context
                .state
                .get_checkpoint_for_predicate(clean_nogood[1])
                .unwrap()
        } else {
            // For unit nogoods, the solver backtracks to the root level.
            0
        };

        Self {
            predicates: clean_nogood,
            backjump_level,
        }
    }
}
