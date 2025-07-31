use crate::branching::branchers::alternating::AlternatingStrategy;
use crate::branching::branchers::alternating::BrancherToUse;
use crate::branching::Brancher;
use crate::branching::BrancherEvent;
use crate::branching::SelectionContext;
use crate::results::SolutionReference;

/// Specifies that the [`AlternatingBrancher`] should use the provided [`Strategy`] until a solution
/// is found, after which it will use [`DefaultBrancher`].
#[derive(Debug)]
pub struct UntilSolution<Strategy> {
    strategy_before_solution: Strategy,
    has_found_solution: bool,
}

impl<Strategy> UntilSolution<Strategy> {
    pub fn new(strategy: Strategy) -> Self {
        Self {
            strategy_before_solution: strategy,
            has_found_solution: false,
        }
    }
}

impl<Strategy: AlternatingStrategy> AlternatingStrategy for UntilSolution<Strategy> {
    fn next_decision(&mut self, context: &mut SelectionContext) -> BrancherToUse {
        if self.has_found_solution {
            // If we have found a solution then we use the default
            BrancherToUse::Default
        } else {
            // Othterwise, we use the other strategy
            self.strategy_before_solution.next_decision(context)
        }
    }

    fn is_using_default_brancher(&self) -> bool {
        // If we have found a solution then we always use the default brancher; if not, then we use
        // the default brancher if the other strategy uses the default brancher
        self.has_found_solution || self.strategy_before_solution.is_using_default_brancher()
    }

    fn subscribe_to_events(&self) -> Vec<BrancherEvent> {
        vec![BrancherEvent::Solution]
            .into_iter()
            .chain(self.strategy_before_solution.subscribe_to_events())
            .collect()
    }

    fn on_solution(&mut self, _solution: SolutionReference) {
        // We will not need to update the other strategy anymore since it is not used
        self.has_found_solution = true;
    }

    fn on_restart(&mut self) {
        if !self.has_found_solution {
            // If we have not yet found a solution, we notify the other strategy of the restart
            self.strategy_before_solution.on_restart();
        }
    }

    fn is_restart_pointless(
        &mut self,
        default_brancher: &mut impl Brancher,
        other_brancher: &mut impl Brancher,
    ) -> bool {
        if self.has_found_solution {
            // If we have found a solution then we let the default brancher indicate
            default_brancher.is_restart_pointless()
        } else {
            // Otherwise we defer to the other strategy
            self.strategy_before_solution
                .is_restart_pointless(default_brancher, other_brancher)
        }
    }

    fn will_always_use_default(&self) -> bool {
        // If we have found a solution then we always use the default brancher; if not, then we
        // always use the default brancher if the other strategy always uses the default brancher
        self.has_found_solution || self.strategy_before_solution.will_always_use_default()
    }
}

#[cfg(test)]
mod tests {
    use crate::basic_types::tests::TestRandom;
    use crate::branching::branchers::alternating::every_x_restarts::EveryXRestarts;
    use crate::branching::branchers::alternating::other_only::OtherOnly;
    use crate::branching::branchers::alternating::until_solution::UntilSolution;
    use crate::branching::branchers::alternating::AlternatingBrancher;
    use crate::branching::Brancher;
    use crate::branching::SelectionContext;
    use crate::engine::Assignments;
    use crate::results::Solution;
    use crate::results::SolutionReference;
    use crate::Solver;

    #[test]
    fn test_switch_to_default_after_first_solution() {
        let solver = Solver::default();
        let mut brancher = AlternatingBrancher::new(
            &solver,
            solver.default_brancher(),
            UntilSolution::new(OtherOnly),
        );

        let assignments = Assignments::default();
        let empty_solution_reference = SolutionReference::new(&assignments);

        assert!(!brancher.is_using_default_brancher());
        brancher.on_solution(empty_solution_reference);
        assert!(brancher.is_using_default_brancher());
        brancher.on_solution(empty_solution_reference);
        assert!(brancher.is_using_default_brancher());
        brancher.on_solution(empty_solution_reference);
        assert!(brancher.is_using_default_brancher());
    }

    #[test]
    fn test_switch_after_first_solution() {
        let assignments = Assignments::default();
        let solver = Solver::default();
        let mut brancher = AlternatingBrancher::new(
            &solver,
            solver.default_brancher(),
            UntilSolution::new(OtherOnly),
        );

        assert!(!brancher.is_using_default_brancher());
        brancher.on_restart();
        // next_decision is called to ensure that the brancher has actually switched
        let _ = brancher.next_decision(&mut SelectionContext::new(
            &assignments,
            &mut TestRandom::default(),
        ));
        assert!(!brancher.is_using_default_brancher());

        brancher.on_restart();
        let _ = brancher.next_decision(&mut SelectionContext::new(
            &assignments,
            &mut TestRandom::default(),
        ));
        assert!(!brancher.is_using_default_brancher());

        brancher.on_solution(Solution::new(assignments.clone()).as_reference());
        let _ = brancher.next_decision(&mut SelectionContext::new(
            &assignments,
            &mut TestRandom::default(),
        ));
        assert!(brancher.is_using_default_brancher());

        brancher.on_restart();
        let _ = brancher.next_decision(&mut SelectionContext::new(
            &assignments,
            &mut TestRandom::default(),
        ));
        assert!(brancher.is_using_default_brancher());

        brancher.on_restart();
        let _ = brancher.next_decision(&mut SelectionContext::new(
            &assignments,
            &mut TestRandom::default(),
        ));
        assert!(brancher.is_using_default_brancher());

        brancher.on_solution(Solution::new(assignments.clone()).as_reference());
        let _ = brancher.next_decision(&mut SelectionContext::new(
            &assignments,
            &mut TestRandom::default(),
        ));
        assert!(brancher.is_using_default_brancher());
    }

    #[test]
    fn test_every_restart_until_first_solution() {
        let assignments = Assignments::default();
        let solver = Solver::default();
        let mut brancher = AlternatingBrancher::new(
            &solver,
            solver.default_brancher(),
            UntilSolution::new(EveryXRestarts::new(1)),
        );

        assert!(!brancher.is_using_default_brancher());
        brancher.on_restart();
        // next_decision is called to ensure that the brancher has actually switched
        let _ = brancher.next_decision(&mut SelectionContext::new(
            &assignments,
            &mut TestRandom::default(),
        ));
        assert!(brancher.is_using_default_brancher());

        brancher.on_restart();
        let _ = brancher.next_decision(&mut SelectionContext::new(
            &assignments,
            &mut TestRandom::default(),
        ));
        assert!(!brancher.is_using_default_brancher());

        brancher.on_solution(Solution::new(assignments.clone()).as_reference());
        let _ = brancher.next_decision(&mut SelectionContext::new(
            &assignments,
            &mut TestRandom::default(),
        ));
        assert!(brancher.is_using_default_brancher());

        brancher.on_restart();
        let _ = brancher.next_decision(&mut SelectionContext::new(
            &assignments,
            &mut TestRandom::default(),
        ));
        assert!(brancher.is_using_default_brancher());

        brancher.on_restart();
        let _ = brancher.next_decision(&mut SelectionContext::new(
            &assignments,
            &mut TestRandom::default(),
        ));
        assert!(brancher.is_using_default_brancher());

        brancher.on_solution(Solution::new(assignments.clone()).as_reference());
        let _ = brancher.next_decision(&mut SelectionContext::new(
            &assignments,
            &mut TestRandom::default(),
        ));
        assert!(brancher.is_using_default_brancher());
    }
}
