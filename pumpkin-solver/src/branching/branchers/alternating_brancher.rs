//! A [`Brancher`] which alternates between the [`DefaultBrancher`] and another [`Brancher`] based
//! on the strategy specified in [`AlternatingStrategy`].

use crate::basic_types::SolutionReference;
use crate::branching::brancher::BrancherEvents;
use crate::branching::Brancher;
use crate::branching::SelectionContext;
use crate::engine::predicates::predicate::Predicate;
use crate::engine::variables::DomainId;
use crate::engine::Assignments;
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
            default_brancher: solver.default_brancher(),
            strategy,
            has_considered_restart: false,
        }
    }

    /// Toggles which [`Brancher`] is used.
    fn toggle_brancher(&mut self) {
        self.is_using_default_brancher = !self.is_using_default_brancher
    }

    /// Returns true if only the default strategy is used from now on and false otherwise.
    ///
    /// This is important if [`AlternatingStrategy::SwitchToDefaultAfterFirstSolution`] is used as
    /// the strategy.
    fn will_always_use_default(&self) -> bool {
        match self.strategy {
            AlternatingStrategy::SwitchToDefaultAfterFirstSolution => {
                self.is_using_default_brancher
            }
            _ => false,
        }
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

    fn on_appearance_in_conflict_predicate(&mut self, predicate: Predicate) {
        self.default_brancher
            .on_appearance_in_conflict_predicate(predicate);
        if !self.will_always_use_default() {
            self.other_brancher
                .on_appearance_in_conflict_predicate(predicate)
        }
    }

    fn on_conflict(&mut self) {
        self.default_brancher.on_conflict();
        if !self.will_always_use_default() {
            self.other_brancher.on_conflict();
        }
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
                // Switch only the first time, note that `even_number_of_solutions` is initialised
                // to true
                if self.even_number_of_solutions {
                    self.even_number_of_solutions = false;
                    self.is_using_default_brancher = true;
                }
            }
            _ => {}
        }

        self.default_brancher.on_solution(solution);
        if !self.will_always_use_default() {
            self.other_brancher.on_solution(solution);
        }
    }

    fn on_unassign_integer(&mut self, variable: DomainId, value: i32) {
        self.default_brancher.on_unassign_integer(variable, value);
        if !self.will_always_use_default() {
            self.other_brancher.on_unassign_integer(variable, value)
        }
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

    fn on_backtrack(&mut self) {
        self.default_brancher.on_backtrack();
        if !self.will_always_use_default() {
            self.other_brancher.on_backtrack();
        }
    }

    fn synchronise(&mut self, assignments: &Assignments) {
        self.default_brancher.synchronise(assignments);
        if !self.will_always_use_default() {
            self.other_brancher.synchronise(assignments);
        }
    }

    fn get_relevant_brancher_events(&self) -> Vec<BrancherEvents> {
        // We require the restart event and on solution event for the alternating brancher itself;
        // additionally, it will be interested in the events of its sub-branchers
        [BrancherEvents::Restart, BrancherEvents::Solution]
            .into_iter()
            .chain(self.default_brancher.get_relevant_brancher_events())
            .chain(self.other_brancher.get_relevant_brancher_events())
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::AlternatingBrancher;
    use super::AlternatingStrategy;
    use crate::basic_types::tests::TestRandom;
    use crate::branching::Brancher;
    use crate::branching::SelectionContext;
    use crate::engine::Assignments;
    use crate::results::SolutionReference;
    use crate::Solver;

    #[test]
    fn test_every_solution() {
        let solver = Solver::default();
        let mut brancher = AlternatingBrancher::new(
            &solver,
            solver.default_brancher(),
            AlternatingStrategy::EverySolution,
        );

        let assignments = Assignments::default();
        let empty_solution_reference = SolutionReference::new(&assignments);

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
            solver.default_brancher(),
            AlternatingStrategy::EveryOtherSolution,
        );

        let assignments = Assignments::default();
        let empty_solution_reference = SolutionReference::new(&assignments);

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
            solver.default_brancher(),
            AlternatingStrategy::SwitchToDefaultAfterFirstSolution,
        );

        let assignments = Assignments::default();
        let empty_solution_reference = SolutionReference::new(&assignments);

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
        let assignments = Assignments::default();
        let solver = Solver::default();
        let mut brancher = AlternatingBrancher::new(
            &solver,
            solver.default_brancher(),
            AlternatingStrategy::EveryRestart,
        );

        assert!(!brancher.is_using_default_brancher);
        brancher.on_restart();
        // next_decision is called to ensure that the brancher has actually switched
        let _ = brancher.next_decision(&mut SelectionContext::new(
            &assignments,
            &mut TestRandom::default(),
        ));
        assert!(brancher.is_using_default_brancher);

        brancher.on_restart();
        let _ = brancher.next_decision(&mut SelectionContext::new(
            &assignments,
            &mut TestRandom::default(),
        ));
        assert!(!brancher.is_using_default_brancher);

        brancher.on_restart();
        let _ = brancher.next_decision(&mut SelectionContext::new(
            &assignments,
            &mut TestRandom::default(),
        ));

        assert!(brancher.is_using_default_brancher);
    }
}
