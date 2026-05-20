use std::ops::Deref;

use crate::conflict_resolving::ConflictAnalysisContext;
use crate::engine::ConflictResolverType;
use crate::predicates::Predicate;
use crate::pumpkin_assert_moderate;

/// A structure which stores a learned nogood
///
/// There are two assumptions:
/// - The asserting literal (i.e. the literal of the current decision level) is placed at the `0`th
///   index of [`LearnedNogood::literals`].
/// - A literal from the second-highest decision level is placed at the `1`st index of
///   [`LearnedNogood::literals`].
#[derive(Clone, Debug)]
pub(crate) struct LearnedNogood {
    pub(crate) predicates: Vec<Predicate>,
    pub(crate) backtrack_level: usize,
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
    ///
    /// When using [`ConflictResolverType::ExtendedUIP`], it is ensured that the [`Predicate`] at
    /// the the 1-st position is the one with the highest decision level concerning a different
    /// domain than the propagating domain
    pub(crate) fn create_from_vec(
        mut clean_nogood: Vec<Predicate>,
        context: &ConflictAnalysisContext,
        analysis_mode: ConflictResolverType,
    ) -> Self {
        if clean_nogood.is_empty() {
            return Self {
                predicates: vec![],
                backtrack_level: 0,
            };
        }

        // We perform a linear scan to maintain the two invariants:
        // - The predicate from the current decision level is placed at index 0
        // - The predicate from the highest decision level below the current is placed at index 1

        // If we are performing extended conflict analysis, then we find the domain that is
        // currently propagating; this is to correctly put the predicates in the right place.
        let propagating_domain = if matches!(
            analysis_mode,
            ConflictResolverType::ExtendedCPIP | ConflictResolverType::BoundsExtendedCPIP
        ) {
            Some(
                clean_nogood
                    .iter()
                    .find(|predicate| {
                        context
                            .state
                            .get_checkpoint_for_predicate(**predicate)
                            .is_some_and(|checkpoint| checkpoint == context.state.get_checkpoint())
                    })
                    .copied()
                    .expect("Expected at least one element to be from the current decision level"),
            )
        } else {
            None
        };

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
                if propagating_domain.is_none()
                    || clean_nogood[0].get_domain() != clean_nogood[index].get_domain()
                {
                    index -= 1;
                }
            } else if dl > highest_level_below_current
                && (propagating_domain.is_none()
                    || predicate.get_domain() != propagating_domain.unwrap().get_domain())
            {
                // The extra condition is to ensure that the first two predicates in the nogood
                // are over different variables.
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

        pumpkin_assert_moderate!(
            propagating_domain.is_none()
                || backjump_level == {
                    // In this case, the order of the predicates is irrelevant (for now)
                    //
                    // We first find the propagating domain
                    let propagating_domain = clean_nogood[0].get_domain();
                    // Then we calculate the backjump level as the level of the highest present
                    // predicate in the nogood which is not concerning the
                    // propagating domain
                    clean_nogood
                        .iter()
                        .filter(|predicate| predicate.get_domain() != propagating_domain)
                        .map(|predicate| {
                            context
                                .state
                                .get_checkpoint_for_predicate(*predicate)
                                .unwrap()
                        })
                        .max()
                        .unwrap_or(0)
                },
            "The predicate of the second-highest decision level was not correctly placed when using extended conflict analysis."
        );

        Self {
            predicates: clean_nogood,
            backtrack_level: backjump_level,
        }
    }
}
