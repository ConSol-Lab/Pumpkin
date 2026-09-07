use pumpkin_core::branching::BrancherEvent;
use pumpkin_core::branching::SelectionContext;
use pumpkin_core::results::SolutionReference;

#[cfg(doc)]
use crate::branchers::alternating::AlternatingBrancher;
use crate::branchers::alternating::BrancherToUse;
use crate::branchers::alternating::strategies::AlternatingStrategy;

/// Specifies that the [`AlternatingBrancher`] should switch between
/// [`BrancherToUse::Default`] and the provided brancher every `x`th solution.
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
    use pumpkin_core::Solver;
    use pumpkin_core::branching::Brancher;

    use crate::DefaultBrancher;
    use crate::branchers::alternating::alternating_brancher::AlternatingBrancher;
    use crate::branchers::alternating::strategies::every_x_solutions::EveryXSolutions;

    #[test]
    fn test_every_other_solution() {
        let solver = Solver::default();
        let mut brancher = AlternatingBrancher::new(
            &solver,
            DefaultBrancher::default_over_all_variables(solver.get_domains()),
            EveryXSolutions::new(2),
        );

        let empty_solution_reference = solver.get_solution_reference();

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
