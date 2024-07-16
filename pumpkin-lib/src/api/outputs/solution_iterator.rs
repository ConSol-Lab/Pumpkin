//! Contains the structures corresponding to solution iterations.

use super::SolutionReference;
use crate::basic_types::CSPSolverExecutionFlag;
use crate::branching::Brancher;
use crate::engine::ConstraintSatisfactionSolver;
use crate::predicate;
use crate::predicates::Predicate;
use crate::termination::TerminationCondition;
#[cfg(doc)]
use crate::Solver;

/// A struct which allows the retrieval of multiple solutions to a satisfaction problem.
#[derive(Debug)]
pub struct SolutionIterator<'solver, 'brancher, 'termination, B: Brancher, T> {
    solver: &'solver mut ConstraintSatisfactionSolver,
    brancher: &'brancher mut B,
    termination: &'termination mut T,
    next_blocking_clause: Option<Vec<Predicate>>,
    has_solution: bool,
}

impl<'solver, 'brancher, 'termination, B: Brancher, T: TerminationCondition>
    SolutionIterator<'solver, 'brancher, 'termination, B, T>
{
    pub(crate) fn new(
        solver: &'solver mut ConstraintSatisfactionSolver,
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
            self.solver.restore_state_at_root(self.brancher);
            if self.solver.add_clause(blocking_clause).is_err() {
                return IteratedSolution::Finished;
            }
        }
        match self.solver.solve(self.termination, self.brancher) {
            CSPSolverExecutionFlag::Feasible => {
                self.has_solution = true;
                self.brancher
                    .on_solution(self.solver.get_solution_reference());
                let solution = self.solver.get_solution_reference();
                self.next_blocking_clause = Some(self.get_blocking_clause());
                IteratedSolution::Solution(solution)
            }
            CSPSolverExecutionFlag::Infeasible if !self.has_solution => {
                IteratedSolution::Unsatisfiable
            }
            CSPSolverExecutionFlag::Infeasible => IteratedSolution::Finished,
            CSPSolverExecutionFlag::Timeout => IteratedSolution::Unknown,
        }
    }

    /// Creates a clause which prevents the current solution from occurring again by going over the
    /// defined output variables and creating a clause which prevents those values from
    /// being assigned. This method is used when attempting to find multiple solutions. It restores
    /// the state of the passed [`ConstraintSatisfactionSolver`] to the root (using
    /// [`ConstraintSatisfactionSolver::restore_state_at_root`]) and returns true if adding the
    /// clause was successful (i.e. it is possible that there could be another solution) and
    /// returns false otherwise (i.e. if adding a clause led to a conflict which indicates that
    /// there are no more solutions).
    fn get_blocking_clause(&self) -> Vec<Predicate> {
        self.solver
            .assignments
            .get_domains()
            .map(|variable| {
                predicate!(
                    variable
                        != self
                            .solver
                            .assignments
                            .get_assigned_value(variable)
                            .expect("Expected every variable to be assigned in a solution")
                )
            })
            .collect::<Vec<_>>()
    }
}

/// Enum which specifies the status of the call to [`SolutionIterator::next_solution`].
#[allow(clippy::large_enum_variant)]
#[derive(Debug)]
pub enum IteratedSolution<'solver> {
    /// A new solution was identified.
    Solution(SolutionReference<'solver>),

    /// No more solutions exist.
    Finished,

    /// The solver was terminated during search.
    Unknown,

    /// There exists no solution
    Unsatisfiable,
}
