#[cfg(doc)]
use crate::branching::alternating::AlternatingBrancher;
use crate::branching::branchers::alternating::strategies::AlternatingStrategy;
use crate::branching::BrancherEvent;
use crate::branching::SelectionContext;
use crate::results::SolutionReference;
#[cfg(doc)]
use crate::DefaultBrancher;

/// Specifies that the [`AlternatingBrancher`] should switch to [`DefaultBrancher`] for the
/// rest of the search after finding a single solution with the provided strategy.
#[derive(Default, Debug, Clone, Copy)]
pub struct DefaultAfterFirstSolution {
    has_found_solution: bool,
}

impl AlternatingStrategy for DefaultAfterFirstSolution {
    fn next_decision(&mut self, _context: &mut SelectionContext) -> bool {
        self.has_found_solution
    }

    fn subscribe_to_events(&self) -> Vec<BrancherEvent> {
        vec![BrancherEvent::Solution]
    }

    fn on_solution(&mut self, _solution: SolutionReference) {
        self.has_found_solution = true;
    }

    fn will_always_use_default(&self) -> bool {
        self.has_found_solution
    }

    fn is_using_default_brancher(&self) -> bool {
        self.has_found_solution
    }
}

#[cfg(test)]
mod tests {
    use crate::basic_types::tests::TestRandom;
    use crate::branching::branchers::alternating::alternating_brancher::AlternatingBrancher;
    use crate::branching::branchers::alternating::strategies::default_after_first_solution::DefaultAfterFirstSolution;
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
            DefaultAfterFirstSolution::default(),
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
            DefaultAfterFirstSolution::default(),
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
}
