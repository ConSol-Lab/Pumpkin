//! Contains the structures corresponding to solution iterations.

use super::SatisfactionResult::Satisfiable;
use super::SatisfactionResult::Unknown;
use super::SatisfactionResult::Unsatisfiable;
use crate::branching::Brancher;
use crate::predicate;
use crate::predicates::Predicate;
use crate::results::ProblemSolution;
use crate::results::Solution;
use crate::termination::TerminationCondition;
use crate::Solver;

/// A struct which allows the retrieval of multiple solutions to a satisfaction problem.
#[derive(Debug)]
pub struct SolutionIterator<'solver, 'brancher, 'termination, B: Brancher, T> {
    solver: &'solver mut Solver,
    brancher: &'brancher mut B,
    termination: &'termination mut T,
    next_blocking_clause: Option<Vec<Predicate>>,
    has_solution: bool,
}

impl<'solver, 'brancher, 'termination, B: Brancher, T: TerminationCondition>
    SolutionIterator<'solver, 'brancher, 'termination, B, T>
{
    pub(crate) fn new(
        solver: &'solver mut Solver,
        brancher: &'brancher mut B,
        termination: &'termination mut T,
    ) -> Self {
        SolutionIterator {
            solver,
            brancher,
            termination,
            next_blocking_clause: None,
            has_solution: false,
        }
    }

    /// Find a new solution by blocking the previous solution from being found. Also calls the
    /// [`Brancher::on_solution`] method from the [`Brancher`] used to run the initial solve.
    pub fn next_solution(&mut self) -> IteratedSolution {
        if let Some(blocking_clause) = self.next_blocking_clause.take() {
            if self.solver.add_clause(blocking_clause).is_err() {
                return IteratedSolution::Finished;
            }
        }

        match self.solver.satisfy(self.brancher, self.termination) {
            Satisfiable(solution) => {
                self.has_solution = true;
                self.next_blocking_clause = Some(get_blocking_clause(&solution));
                IteratedSolution::Solution(solution, self.solver)
            }
            Unsatisfiable => {
                if self.has_solution {
                    IteratedSolution::Finished
                } else {
                    IteratedSolution::Unsatisfiable
                }
            }
            Unknown => IteratedSolution::Unknown,
        }
    }
}

/// Creates a clause which prevents the current solution from occurring again by going over the
/// defined output variables and creating a clause which prevents those values from
/// being assigned.
///
/// This method is used when attempting to find multiple solutions.
fn get_blocking_clause(solution: &Solution) -> Vec<Predicate> {
    solution
        .get_domains()
        .map(|variable| predicate!(variable != solution.get_integer_value(variable)))
        .collect::<Vec<_>>()
}
/// Enum which specifies the status of the call to [`SolutionIterator::next_solution`].
#[allow(
    clippy::large_enum_variant,
    reason = "these will not be stored in bulk, so this is not an issue"
)]
#[derive(Debug)]
pub enum IteratedSolution<'a> {
    /// A new solution was identified.
    Solution(Solution, &'a Solver),

    /// No more solutions exist.
    Finished,

    /// The solver was terminated during search.
    Unknown,

    /// There exists no solution
    Unsatisfiable,
}
