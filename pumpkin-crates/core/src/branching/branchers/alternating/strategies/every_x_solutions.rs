use crate::branching::branchers::alternating::strategies::AlternatingStrategy;
use crate::branching::branchers::alternating::BrancherToUse;
use crate::branching::BrancherEvent;
use crate::branching::SelectionContext;
use crate::results::SolutionReference;

/// Specifies that the [`AlternatingBrancher`] should switch between [`DefaultBrancher`] and
/// the provided brancher every `x`th solution.
#[derive(Debug, Clone, Copy)]
pub struct EveryXSolutions {
    use_default_brancher: bool,
    x: u32,
    num_solutions: u32,
}

impl EveryXSolutions {
    pub fn new(x: u32) -> Self {
        Self {
            use_default_brancher: false,
            x,
            num_solutions: 0,
        }
    }
}

impl AlternatingStrategy for EveryXSolutions {
    fn next_decision(&mut self, _context: &mut SelectionContext) -> BrancherToUse {
        if self.use_default_brancher {
            BrancherToUse::Default
        } else {
            BrancherToUse::Other
        }
    }

    fn subscribe_to_events(&self) -> Vec<BrancherEvent> {
        vec![BrancherEvent::Solution]
    }

    fn on_solution(&mut self, _solution: SolutionReference) {
        self.num_solutions += 1;
        if self.num_solutions.is_multiple_of(self.x) {
            self.use_default_brancher = !self.use_default_brancher
        }
    }

    fn is_using_default_brancher(&self) -> bool {
        self.use_default_brancher
    }
}

#[cfg(test)]
mod tests {
    use crate::branching::branchers::alternating::alternating_brancher::AlternatingBrancher;
    use crate::branching::branchers::alternating::strategies::every_x_solutions::EveryXSolutions;
    use crate::branching::Brancher;
    use crate::engine::Assignments;
    use crate::results::SolutionReference;
    use crate::Solver;

    #[test]
    fn test_every_other_solution() {
        let solver = Solver::default();
        let mut brancher =
            AlternatingBrancher::new(&solver, solver.default_brancher(), EveryXSolutions::new(2));

        let assignments = Assignments::default();
        let empty_solution_reference = SolutionReference::new(&assignments);

        assert!(!brancher.is_using_default_brancher());
        brancher.on_solution(empty_solution_reference);
        assert!(!brancher.is_using_default_brancher());
        brancher.on_solution(empty_solution_reference);
        assert!(brancher.is_using_default_brancher());
        brancher.on_solution(empty_solution_reference);
        assert!(brancher.is_using_default_brancher());
        brancher.on_solution(empty_solution_reference);
        assert!(!brancher.is_using_default_brancher());
    }
}
