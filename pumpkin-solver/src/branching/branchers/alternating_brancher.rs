//! A [`Brancher`] which alternates between the [`DefaultBrancher`] and another [`Brancher`] based
//! on the strategy specified in [`AlternatingStrategy`].

use crate::basic_types::SolutionReference;
use crate::branching::Brancher;
use crate::branching::SelectionContext;
#[cfg(doc)]
use crate::branching::SolutionGuidedValueSelector;
use crate::engine::predicates::predicate::Predicate;
use crate::engine::variables::DomainId;
use crate::engine::variables::Literal;
use crate::DefaultBrancher;
use crate::Solver;

/// Determines which alternation strategy is used by the [`AlternatingBrancher`].
///
/// Currently we allow switching every time a solution is found
/// ([`AlternatingStrategy::EverySolution`]), after every other solution
/// ([`AlternatingStrategy::EveryOtherSolution`]), switching to [`DefaultBrancher`] after the first
/// solution is found and switching strategy upon restart.
#[derive(Debug, Clone, Copy, PartialEq)]
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
/// provided [`AlternatingStrategy`].
///
/// Note that the [`DefaultBrancher`] is informed of every
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
    /// Indicates that the [`AlternatingBrancher`] has considered a restart; note that this
    /// variable is only used in the context of [`ÀlternatingStrategy::EveryRestart`].
    has_considered_restart: bool,
}

impl<OtherBrancher: Brancher> AlternatingBrancher<OtherBrancher> {
    pub fn new(
        solver: &Solver,
        other_brancher: OtherBrancher,
        strategy: AlternatingStrategy,
    ) -> Self {
        Self {
            even_number_of_solutions: true,
            is_using_default_brancher: false,
            other_brancher,
            default_brancher: solver.default_brancher_over_all_propositional_variables(),
            strategy,
            has_considered_restart: false,
        }
    }

    /// Toggles which [`Brancher`] is used.
    fn toggle_brancher(&mut self) {
        self.is_using_default_brancher = !self.is_using_default_brancher
    }
}

impl<OtherBrancher: Brancher> Brancher for AlternatingBrancher<OtherBrancher> {
    fn next_decision(&mut self, context: &mut SelectionContext) -> Option<Predicate> {
        // If we have considered a restart and the AlternatingStrategy relies on restarts then we
        // toggle the brancher and set the variable to false
        if self.has_considered_restart && self.strategy == AlternatingStrategy::EveryRestart {
            self.has_considered_restart = false;
            self.toggle_brancher();
        }

        if self.is_using_default_brancher {
            self.default_brancher.next_decision(context)
        } else {
            self.other_brancher.next_decision(context)
        }
    }

    fn on_appearance_in_conflict_integer(&mut self, variable: DomainId) {
        self.other_brancher
            .on_appearance_in_conflict_integer(variable)
    }

    fn on_appearance_in_conflict_literal(&mut self, literal: Literal) {
        self.other_brancher
            .on_appearance_in_conflict_literal(literal);
        self.default_brancher
            .on_appearance_in_conflict_literal(literal)
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

    fn on_unassign_literal(&mut self, literal: Literal) {
        self.other_brancher.on_unassign_literal(literal);
        self.default_brancher.on_unassign_literal(literal)
    }

    fn on_restart(&mut self) {
        if self.strategy == AlternatingStrategy::EveryRestart {
            // We have considered a restart and we should switch
            self.has_considered_restart = true;
        }
    }

    fn is_restart_pointless(&mut self) -> bool {
        match self.strategy {
            AlternatingStrategy::EveryRestart => {
                // We indicate that we have considered a restart, this can then be used by the
                // EveryRestart AlternatingStrategy to determine when to switch
                self.has_considered_restart = true;

                // In the case of the `EveryRestart` strategy, note that we switch to the other
                // strategy and then the restart is performed so we check whether restarting for the
                // other brancher is pointless
                if self.is_using_default_brancher {
                    self.other_brancher.is_restart_pointless()
                } else {
                    self.default_brancher.is_restart_pointless()
                }
            }
            _ => {
                if self.is_using_default_brancher {
                    self.default_brancher.is_restart_pointless()
                } else {
                    self.other_brancher.is_restart_pointless()
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::AlternatingBrancher;
    use super::AlternatingStrategy;
    use crate::basic_types::tests::TestRandom;
    use crate::branching::Brancher;
    use crate::branching::SelectionContext;
    use crate::engine::AssignmentsInteger;
    use crate::engine::AssignmentsPropositional;
    use crate::results::SolutionReference;
    use crate::Solver;

    #[test]
    fn test_every_solution() {
        let solver = Solver::default();
        let mut brancher = AlternatingBrancher::new(
            &solver,
            solver.default_brancher_over_all_propositional_variables(),
            AlternatingStrategy::EverySolution,
        );

        let assignments_propositional = AssignmentsPropositional::default();
        let assignments_integer = AssignmentsInteger::default();
        let empty_solution_reference =
            SolutionReference::new(&assignments_propositional, &assignments_integer);

        assert!(!brancher.is_using_default_brancher);
        brancher.on_solution(empty_solution_reference);
        assert!(brancher.is_using_default_brancher);
        brancher.on_solution(empty_solution_reference);
        assert!(!brancher.is_using_default_brancher);
    }

    #[test]
    fn test_every_other_solution() {
        let solver = Solver::default();
        let mut brancher = AlternatingBrancher::new(
            &solver,
            solver.default_brancher_over_all_propositional_variables(),
            AlternatingStrategy::EveryOtherSolution,
        );

        let assignments_propositional = AssignmentsPropositional::default();
        let assignments_integer = AssignmentsInteger::default();
        let empty_solution_reference =
            SolutionReference::new(&assignments_propositional, &assignments_integer);

        assert!(!brancher.is_using_default_brancher);
        brancher.on_solution(empty_solution_reference);
        assert!(!brancher.is_using_default_brancher);
        brancher.on_solution(empty_solution_reference);
        assert!(brancher.is_using_default_brancher);
        brancher.on_solution(empty_solution_reference);
        assert!(brancher.is_using_default_brancher);
        brancher.on_solution(empty_solution_reference);
        assert!(!brancher.is_using_default_brancher);
    }

    #[test]
    fn test_switch_to_default_after_first_solution() {
        let solver = Solver::default();
        let mut brancher = AlternatingBrancher::new(
            &solver,
            solver.default_brancher_over_all_propositional_variables(),
            AlternatingStrategy::SwitchToDefaultAfterFirstSolution,
        );

        let assignments_propositional = AssignmentsPropositional::default();
        let assignments_integer = AssignmentsInteger::default();
        let empty_solution_reference =
            SolutionReference::new(&assignments_propositional, &assignments_integer);

        assert!(!brancher.is_using_default_brancher);
        brancher.on_solution(empty_solution_reference);
        assert!(brancher.is_using_default_brancher);
        brancher.on_solution(empty_solution_reference);
        assert!(brancher.is_using_default_brancher);
        brancher.on_solution(empty_solution_reference);
        assert!(brancher.is_using_default_brancher);
    }

    #[test]
    fn test_every_other_restart() {
        let assignments_integer = AssignmentsInteger::default();
        let assignments_propositional = AssignmentsPropositional::default();

        let solver = Solver::default();
        let mut brancher = AlternatingBrancher::new(
            &solver,
            solver.default_brancher_over_all_propositional_variables(),
            AlternatingStrategy::EveryRestart,
        );

        assert!(!brancher.is_using_default_brancher);
        brancher.on_restart();
        // next_decision is called to ensure that the brancher has actually switched
        let _ = brancher.next_decision(&mut SelectionContext::new(
            &assignments_integer,
            &assignments_propositional,
            &mut TestRandom::default(),
        ));
        assert!(brancher.is_using_default_brancher);

        brancher.on_restart();
        let _ = brancher.next_decision(&mut SelectionContext::new(
            &assignments_integer,
            &assignments_propositional,
            &mut TestRandom::default(),
        ));
        assert!(!brancher.is_using_default_brancher);

        brancher.on_restart();
        let _ = brancher.next_decision(&mut SelectionContext::new(
            &assignments_integer,
            &assignments_propositional,
            &mut TestRandom::default(),
        ));

        assert!(brancher.is_using_default_brancher);
    }
}
