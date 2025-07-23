use crate::branching::branchers::alternating::strategies::AlternatingStrategy;
use crate::branching::BrancherEvent;
use crate::branching::SelectionContext;
use crate::results::SolutionReference;

/// Specifies that the [`AlternatingBrancher`] should switch between [`DefaultBrancher`] and
/// the provided brancher every restart until the first solution is found, after which it will
/// switch to the [`DefaultBrancher`] for the rest of the search.
#[derive(Default, Debug, Clone, Copy)]
pub struct EveryRestartUntilSolution {
    considered_restart: bool,
    has_solution: bool,
    use_default_brancher: bool,
}

impl AlternatingStrategy for EveryRestartUntilSolution {
    fn next_decision(&mut self, _context: &mut SelectionContext) -> bool {
        self.use_default_brancher
    }

    fn is_using_default_brancher(&self) -> bool {
        self.use_default_brancher
    }

    fn subscribe_to_events(&self) -> Vec<BrancherEvent> {
        vec![BrancherEvent::Solution, BrancherEvent::Restart]
    }

    fn on_solution(&mut self, _solution: SolutionReference) {
        self.has_solution = true;
        self.use_default_brancher = true;
    }

    fn on_restart(&mut self) {
        self.considered_restart = true;
    }

    fn is_restart_pointless(
        &mut self,
        default_brancher: &mut impl crate::branching::Brancher,
        other_brancher: &mut impl crate::branching::Brancher,
    ) -> bool {
        // We indicate that we have considered a restart, this can then be used by the
        // AlternatingStrategy to determine when to switch
        self.considered_restart = true;

        // Note that we could switch to the other strategy and then the restart is performed
        // so we check whether restarting for the other brancher is
        // pointless
        if self.use_default_brancher {
            other_brancher.is_restart_pointless()
        } else {
            default_brancher.is_restart_pointless()
        }
    }

    fn will_always_use_default(&self) -> bool {
        self.has_solution
    }
}

#[cfg(test)]
mod tests {
    use crate::basic_types::tests::TestRandom;
    use crate::branching::branchers::alternating::alternating_brancher::AlternatingBrancher;
    use crate::branching::branchers::alternating::strategies::every_restart_until_solution::EveryRestartUntilSolution;
    use crate::branching::Brancher;
    use crate::branching::SelectionContext;
    use crate::engine::Assignments;
    use crate::results::Solution;
    use crate::Solver;

    #[test]
    fn test_every_restart_until_first_solution() {
        let assignments = Assignments::default();
        let solver = Solver::default();
        let mut brancher = AlternatingBrancher::new(
            &solver,
            solver.default_brancher(),
            EveryRestartUntilSolution::default(),
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
