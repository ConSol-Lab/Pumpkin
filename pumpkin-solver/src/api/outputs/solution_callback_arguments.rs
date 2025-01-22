use crate::results::Solution;
use crate::Solver;

/// The input which is passed to the solution callback (which can be set using
/// [`Solver::with_solution_callback`]).
///
/// Provides direct access to the solution via [`SolutionCallbackArguments::solution`] and allows
/// logging the statistics of the [`Solver`] using [`SolutionCallbackArguments::log_statistics`].
#[derive(Debug)]
pub struct SolutionCallbackArguments<'a, 'b> {
    /// The solver which found the solution
    solver: &'a Solver,
    /// The solution which has been found
    pub solution: &'b Solution,
    /// The (optional) objective value provided to the [`Solver`].
    objective_value: Option<i64>,
}

impl<'a, 'b> SolutionCallbackArguments<'a, 'b> {
    pub(crate) fn new(
        solver: &'a Solver,
        solution: &'b Solution,
        objective_value: Option<i64>,
    ) -> Self {
        Self {
            solver,
            solution,
            objective_value,
        }
    }

    /// Log the statistics of the [`Solver`].
    ///
    /// If the solution was found using [`Solver::optimise`] then the
    /// objective value of the current solution is included in the statistics.
    pub fn log_statistics(&self) {
        if let Some(objective_value) = self.objective_value {
            self.solver.log_statistics_with_objective(objective_value)
        } else {
            self.solver.log_statistics()
        }
    }
}
