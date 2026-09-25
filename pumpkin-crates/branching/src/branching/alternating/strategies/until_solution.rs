use pumpkin_core::branching::Brancher;
use pumpkin_core::branching::BrancherEvent;
use pumpkin_core::branching::SelectionContext;
use pumpkin_core::results::SolutionReference;

#[cfg(doc)]
use crate::branching::alternating::AlternatingBrancher;
use crate::branching::alternating::AlternatingStrategy;
use crate::branching::alternating::BrancherToUse;

/// Specifies that the [`AlternatingBrancher`] should use the provided [`AlternatingStrategy`] until
/// a solution is found, after which it will use [`BrancherToUse::Default`].
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
    use pumpkin_core::Solver;
    use pumpkin_core::branching::Brancher;
    use pumpkin_core::branching::SelectionContext;
    use pumpkin_core::results::Solution;
    use pumpkin_core::state::State;

    use crate::DefaultBrancher;
    use crate::branching::alternating::AlternatingBrancher;
    use crate::branching::alternating::every_x_restarts::EveryXRestarts;
    use crate::branching::alternating::other_only::OtherOnly;
    use crate::branching::alternating::until_solution::UntilSolution;
    use crate::testing::TestRandom;

    #[test]
    fn test_switch_to_default_after_first_solution() {
        let solver = Solver::default();
        let mut brancher = AlternatingBrancher::new(
            &solver,
            DefaultBrancher::default_over_all_variables(&solver),
            UntilSolution::new(OtherOnly),
        );

        let empty_solution = Solution::default();

        assert!(!brancher.is_using_default_brancher());
        brancher.on_solution(empty_solution.as_reference());
        assert!(brancher.is_using_default_brancher());
        brancher.on_solution(empty_solution.as_reference());
        assert!(brancher.is_using_default_brancher());
        brancher.on_solution(empty_solution.as_reference());
        assert!(brancher.is_using_default_brancher());
    }

    #[test]
    fn test_switch_after_first_solution() {
        let state = State::default();
        let mut test_rng = TestRandom::default();
        let mut context = SelectionContext::new(&state, &mut test_rng);
        let mut brancher = AlternatingBrancher::new_from_domains(
            state.get_domain_ids(),
            DefaultBrancher::new_from_domains(state.get_domain_ids()),
            UntilSolution::new(OtherOnly),
        );

        assert!(!brancher.is_using_default_brancher());
        brancher.on_restart();
        // next_decision is called to ensure that the brancher has actually switched
        let _ = brancher.next_decision(&mut context);
        assert!(!brancher.is_using_default_brancher());

        brancher.on_restart();
        let _ = brancher.next_decision(&mut context);
        assert!(!brancher.is_using_default_brancher());

        brancher.on_solution(context.solution().as_reference());
        let _ = brancher.next_decision(&mut context);
        assert!(brancher.is_using_default_brancher());

        brancher.on_restart();
        let _ = brancher.next_decision(&mut context);
        assert!(brancher.is_using_default_brancher());

        brancher.on_restart();
        let _ = brancher.next_decision(&mut context);
        assert!(brancher.is_using_default_brancher());

        brancher.on_solution(context.solution().as_reference());
        let _ = brancher.next_decision(&mut context);
        assert!(brancher.is_using_default_brancher());
    }

    #[test]
    fn test_every_restart_until_first_solution() {
        let state = State::default();
        let mut test_rng = TestRandom::default();
        let mut context = SelectionContext::new(&state, &mut test_rng);
        let mut brancher = AlternatingBrancher::new_from_domains(
            state.get_domain_ids(),
            DefaultBrancher::new_from_domains(state.get_domain_ids()),
            UntilSolution::new(EveryXRestarts::new(1)),
        );

        assert!(!brancher.is_using_default_brancher());
        brancher.on_restart();
        // next_decision is called to ensure that the brancher has actually switched
        let _ = brancher.next_decision(&mut context);
        assert!(brancher.is_using_default_brancher());

        brancher.on_restart();
        let _ = brancher.next_decision(&mut context);
        assert!(!brancher.is_using_default_brancher());

        brancher.on_solution(context.solution().as_reference());
        let _ = brancher.next_decision(&mut context);
        assert!(brancher.is_using_default_brancher());

        brancher.on_restart();
        let _ = brancher.next_decision(&mut context);
        assert!(brancher.is_using_default_brancher());

        brancher.on_restart();
        let _ = brancher.next_decision(&mut context);
        assert!(brancher.is_using_default_brancher());

        brancher.on_solution(context.solution().as_reference());
        let _ = brancher.next_decision(&mut context);
        assert!(brancher.is_using_default_brancher());
    }
}
