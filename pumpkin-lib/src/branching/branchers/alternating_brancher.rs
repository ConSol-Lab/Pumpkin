use super::independent_variable_value_brancher::DefaultBrancher;
use super::independent_variable_value_brancher::IndependentVariableValueBrancher;
use crate::basic_types::SolutionReference;
use crate::branching::Brancher;
use crate::branching::SelectionContext;
use crate::engine::predicates::integer_predicate::IntegerPredicate;
use crate::engine::variables::DomainId;
use crate::engine::ConstraintSatisfactionSolver;

/// Determines which alternation strategy is used by the [`AlternatingBrancher`]. Currently we allow
/// switching every time a solution is found ([`AlternatingStrategy::EverySolution`]), after every
/// other solution ([`AlternatingStrategy::EveryOtherSolution`]), switching to [`DefaultBrancher`]
/// after the first solution is found and switching strategy upon restart.
#[derive(Debug, Clone, Copy)]
pub enum AlternatingStrategy {
    /// Specifies that the [`AlternatingBrancher`] should switch between [`DefaultBrancher`] and
    /// the provided brancher every solution.
    EverySolution,
    /// Specifies that the [`AlternatingBrancher`] should switch between [`DefaultBrancher`] and
    /// the provided brancher every other solution.
    EveryOtherSolution,
    /// Specifies that the [`AlternatingBrancher`] should switch to [`DefaultBrancher`] for the
    /// rest of the search after finding a single solution with the provided strategy.
    SwitchToDefaultAfterFirstSolution,
    /// Specifies that the [`AlternatingBrancher`] should switch between [`DefaultBrancher`] and
    /// the provided brancher every restart.
    EveryRestart,
}

/// A [`Brancher`] which switches between its provided brancher and [`DefaultBrancher`] based on the
/// provided [`AlternatingStrategy`]. Note that the [`DefaultBrancher`] is informed of every
/// conflict and unassignment even if it is not currently utilised as [`Brancher`].
///
/// Note that this brancher starts out by using the provided [`Brancher`] and then switches based on
/// the [`AlternatingStrategy`].
#[derive(Debug)]
pub struct AlternatingBrancher<OtherBrancher> {
    even_number_of_solutions: bool,
    /// Whether the [`Brancher`] is currently using the [`DefaultBrancher`] or not
    is_using_default_brancher: bool,
    /// The other [`Brancher`] which is used when
    /// [`AlternatingBrancher::is_using_default_brancher`] is false.
    other_brancher: OtherBrancher,
    /// The instance of [`DefaultBrancher`] which is used when
    /// [`AlternatingBrancher::is_using_default_brancher`] is true.
    default_brancher: DefaultBrancher,
    /// The strategy used to determine when to switch between the two branchers.
    strategy: AlternatingStrategy,
}

impl<OtherBrancher: Brancher> AlternatingBrancher<OtherBrancher> {
    pub fn new(
        solver: &ConstraintSatisfactionSolver,
        other_brancher: OtherBrancher,
        strategy: AlternatingStrategy,
    ) -> Self {
        Self {
            even_number_of_solutions: true,
            is_using_default_brancher: false,
            other_brancher,
            default_brancher: IndependentVariableValueBrancher::default_over_all_variables(solver),
            strategy,
        }
    }

    /// Toggles which [`Brancher`] is used.
    fn toggle_brancher(&mut self) {
        self.is_using_default_brancher = !self.is_using_default_brancher
    }
}

impl<OtherBrancher: Brancher> Brancher for AlternatingBrancher<OtherBrancher> {
    fn next_decision(&mut self, context: &mut SelectionContext) -> Option<IntegerPredicate> {
        if self.is_using_default_brancher {
            self.default_brancher.next_decision(context)
        } else {
            self.other_brancher.next_decision(context)
        }
    }

    fn on_appearance_in_conflict_predicate(&mut self, predicate: IntegerPredicate) {
        self.other_brancher
            .on_appearance_in_conflict_predicate(predicate)
    }

    fn on_conflict(&mut self) {
        self.other_brancher.on_conflict();
        self.default_brancher.on_conflict()
    }

    fn on_solution(&mut self, solution: SolutionReference) {
        match self.strategy {
            AlternatingStrategy::EverySolution => {
                // Switch regardless of how many solutions have been found
                self.toggle_brancher()
            }
            AlternatingStrategy::EveryOtherSolution => {
                self.even_number_of_solutions = !self.even_number_of_solutions;
                // Switch if the number is even -> this leads to switching on every other solution
                if self.even_number_of_solutions {
                    self.toggle_brancher()
                }
            }
            AlternatingStrategy::SwitchToDefaultAfterFirstSolution => {
                // Switch only the first time, not that `even_number_of_solutions` is initialised to
                // true
                if self.even_number_of_solutions {
                    self.even_number_of_solutions = false;
                    self.is_using_default_brancher = true;
                }
            }
            _ => {}
        }

        self.other_brancher.on_solution(solution);
        self.default_brancher.on_solution(solution)
    }

    fn on_unassign_integer(&mut self, variable: DomainId, value: i32) {
        self.other_brancher.on_unassign_integer(variable, value)
    }

    fn on_restart(&mut self) {
        if let AlternatingStrategy::EveryRestart = self.strategy {
            // Switch whenever a restart occurs
            self.toggle_brancher()
        }
    }
}

#[cfg(test)]
mod tests {
    use super::AlternatingBrancher;
    use super::AlternatingStrategy;
    use crate::branching::branchers::independent_variable_value_brancher::IndependentVariableValueBrancher;
    use crate::branching::Brancher;
    use crate::engine::ConstraintSatisfactionSolver;

    #[test]
    fn test_every_solution() {
        let solver = ConstraintSatisfactionSolver::default();
        let mut brancher = AlternatingBrancher::new(
            &solver,
            IndependentVariableValueBrancher::default_over_all_variables(&solver),
            AlternatingStrategy::EverySolution,
        );

        assert!(!brancher.is_using_default_brancher);
        #[allow(deprecated)]
        brancher.on_solution(solver.get_solution_reference());
        assert!(brancher.is_using_default_brancher);
        #[allow(deprecated)]
        brancher.on_solution(solver.get_solution_reference());
        assert!(!brancher.is_using_default_brancher);
    }

    #[test]
    fn test_every_other_solution() {
        let solver = ConstraintSatisfactionSolver::default();
        let mut brancher = AlternatingBrancher::new(
            &solver,
            IndependentVariableValueBrancher::default_over_all_variables(&solver),
            AlternatingStrategy::EveryOtherSolution,
        );

        assert!(!brancher.is_using_default_brancher);
        #[allow(deprecated)]
        brancher.on_solution(solver.get_solution_reference());
        assert!(!brancher.is_using_default_brancher);
        #[allow(deprecated)]
        brancher.on_solution(solver.get_solution_reference());
        assert!(brancher.is_using_default_brancher);
        #[allow(deprecated)]
        brancher.on_solution(solver.get_solution_reference());
        assert!(brancher.is_using_default_brancher);
        #[allow(deprecated)]
        brancher.on_solution(solver.get_solution_reference());
        assert!(!brancher.is_using_default_brancher);
    }

    #[test]
    fn test_switch_to_default_after_first_solution() {
        let solver = ConstraintSatisfactionSolver::default();
        let mut brancher = AlternatingBrancher::new(
            &solver,
            IndependentVariableValueBrancher::default_over_all_variables(&solver),
            AlternatingStrategy::SwitchToDefaultAfterFirstSolution,
        );

        assert!(!brancher.is_using_default_brancher);
        #[allow(deprecated)]
        brancher.on_solution(solver.get_solution_reference());
        assert!(brancher.is_using_default_brancher);
        #[allow(deprecated)]
        brancher.on_solution(solver.get_solution_reference());
        assert!(brancher.is_using_default_brancher);
        #[allow(deprecated)]
        brancher.on_solution(solver.get_solution_reference());
        assert!(brancher.is_using_default_brancher);
    }

    #[test]
    fn test_every_other_restart() {
        let solver = ConstraintSatisfactionSolver::default();
        let mut brancher = AlternatingBrancher::new(
            &solver,
            IndependentVariableValueBrancher::default_over_all_variables(&solver),
            AlternatingStrategy::EveryRestart,
        );

        assert!(!brancher.is_using_default_brancher);
        brancher.on_restart();
        assert!(brancher.is_using_default_brancher);
        brancher.on_restart();
        assert!(!brancher.is_using_default_brancher);
        brancher.on_restart();
        assert!(brancher.is_using_default_brancher);
    }
}
