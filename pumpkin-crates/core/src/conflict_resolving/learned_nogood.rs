use std::ops::Deref;

use crate::conflict_resolving::ConflictAnalysisContext;
use crate::predicates::Predicate;

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
        uses_cpip: bool,
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
        let propagating_domain = if uses_cpip {
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
        let mut backjump_level = if clean_nogood.len() > 1 {
            context
                .state
                .get_checkpoint_for_predicate(clean_nogood[1])
                .unwrap()
        } else {
            // For unit nogoods, the solver backtracks to the root level.
            0
        };

        if clean_nogood.len() > 1 && propagating_domain.is_some() {
            // In this case, the order of the predicates is irrelevant (for now)
            //
            // We first find the propagating domain
            let propagating_domain = clean_nogood[0].get_domain();
            // Then we calculate the backjump level as the level of the highest present predicate
            // in the nogood which is not concerning the propagating domain
            backjump_level = clean_nogood
                .iter()
                .filter(|predicate| predicate.get_domain() != propagating_domain)
                .map(|predicate| {
                    context
                        .state
                        .get_checkpoint_for_predicate(*predicate)
                        .unwrap()
                })
                .max()
                .unwrap_or(0);
        }

        Self {
            predicates: clean_nogood,
            backtrack_level: backjump_level,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::Solver;
    use crate::branching::Brancher;
    use crate::branching::BrancherEvent;
    use crate::branching::SelectionContext;
    use crate::engine::ConflictResolverType;
    use crate::engine::SatisfactionSolverOptions;
    use crate::predicate;
    use crate::predicates::Predicate;
    use crate::proof::InferenceCode;
    use crate::propagation::ReadDomains;

    #[derive(Debug)]
    struct DummyBrancher;

    impl Brancher for DummyBrancher {
        fn next_decision(&mut self, _: &mut SelectionContext) -> Option<Predicate> {
            unreachable!()
        }

        fn subscribe_to_events(&self) -> Vec<BrancherEvent> {
            unreachable!()
        }
    }

    /// A CPIP nogood whose predicates are all over one variable (here `[x >= 3] /\ [x != 5]`)
    /// leads to a backjump to the root, after which those predicates are unassigned. The nogood
    /// should then be added as a permanent nogood and propagated at the root.
    #[test]
    fn cpip_nogood_over_a_single_variable_is_added_after_backjump_to_root() {
        let mut solver = Solver::with_options(SatisfactionSolverOptions {
            analysis_mode: ConflictResolverType::ExtendedCPIP,
            ..Default::default()
        });
        let b = solver.new_bounded_integer(0, 1);
        let x = solver.new_bounded_integer(0, 10);
        let constraint_tag = solver.new_constraint_tag();
        let _ = solver.propagate_to_fixpoint();
        let mut brancher = DummyBrancher;
        #[allow(deprecated, reason = "test")]
        let mut context = solver.conflict_analysis_context(&mut brancher);

        context.new_checkpoint();
        let _ = context.post(predicate!(b == 1)).unwrap();
        let _ = context.post(predicate!(x >= 3)).unwrap();
        let _ = context.post(predicate!(x != 5)).unwrap();

        // The nogood is derived by an inference (e.g., of a propagator), which is logged so that
        // the deduction of the learned nogood can be checked.
        context.log_inference(
            InferenceCode::unknown_label(constraint_tag),
            [predicate!(x >= 3), predicate!(x != 5)],
            None,
        );

        let backjump_level =
            context.process_learned_nogood(vec![predicate!(x >= 3), predicate!(x != 5)], 1, true);

        assert_eq!(backjump_level, 0);
        // The nogood states that x <= 2 or x == 5.
        for value in [3, 4, 6, 7, 8, 9, 10] {
            assert!(
                !context.contains(&x, value),
                "{value} should have been removed"
            );
        }
        assert!(context.contains(&x, 5));
    }
}
