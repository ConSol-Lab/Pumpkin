//! Contains the structures corresponding to solution iterations.

use super::SatisfactionResult::Satisfiable;
use super::SatisfactionResult::Unknown;
use super::SatisfactionResult::Unsatisfiable;
use super::SolutionReference;
use crate::branching::Brancher;
use crate::engine::propagation::propagation_context::HasAssignments;
use crate::results::Solution;
use crate::termination::TerminationCondition;
use crate::variables::Literal;
use crate::Solver;

/// A struct which allows the retrieval of multiple solutions to a satisfaction problem.
#[derive(Debug)]
pub struct SolutionIterator<'solver, 'brancher, 'termination, B: Brancher, T> {
    solver: &'solver mut Solver,
    brancher: &'brancher mut B,
    termination: &'termination mut T,
    next_blocking_clause: Option<Vec<Literal>>,
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
            self.solver
                .get_satisfaction_solver_mut()
                .restore_state_at_root(self.brancher);
            if self.solver.add_clause(blocking_clause).is_err() {
                return IteratedSolution::Finished;
            }
        }
        match self.solver.satisfy(self.brancher, self.termination) {
            Satisfiable(solution) => {
                self.has_solution = true;
                self.next_blocking_clause = Some(get_blocking_clause(&solution));
                self.solver.process_solution(&solution, self.brancher);
                IteratedSolution::Solution(self.solver.get_solution_reference())
            }
            Unsatisfiable => IteratedSolution::Unsatisfiable,
            Unknown => IteratedSolution::Unknown,
            // CSPSolverExecutionFlag::Feasible => {
            //    self.has_solution = true;
            //    self.brancher
            //        .on_solution(self.solver.get_solution_reference());
            //    let solution = self.solver.get_solution_reference();
            //    self.next_blocking_clause = Some(self.get_blocking_clause());
            //    IteratedSolution::Solution(solution)
            //}
            // CSPSolverExecutionFlag::Infeasible if !self.has_solution => {
            //    IteratedSolution::Unsatisfiable
            //}
            // CSPSolverExecutionFlag::Infeasible => IteratedSolution::Finished,
            // CSPSolverExecutionFlag::Timeout => IteratedSolution::Unknown,
        }
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
fn get_blocking_clause(solution: &Solution) -> Vec<Literal> {
    solution
        .assignments_propositional()
        .get_propositional_variables()
        .filter(|propositional_variable| {
            solution
                .assignments_propositional()
                .is_variable_assigned(*propositional_variable)
        })
        .map(|propositional_variable| {
            !Literal::new(
                propositional_variable,
                solution
                    .assignments_propositional()
                    .is_variable_assigned_true(propositional_variable),
            )
        })
        .collect::<Vec<_>>()
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
