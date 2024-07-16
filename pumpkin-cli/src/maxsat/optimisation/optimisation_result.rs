use pumpkin_lib::results::Solution;

#[cfg(doc)]
use super::optimisation_solver::OptimisationSolver;

/// The result of calling [`OptimisationSolver::solve()`].
#[derive(Debug)]
pub(crate) enum MaxSatOptimisationResult {
    /// There exists no solution with a better objective value than this one.
    #[allow(dead_code)]
    Optimal { solution: Solution },
    /// The optimal solution was not found within the time budget. However, at least one solution
    /// was found. The provided solution is the solution with the best objective value that was
    /// encountered.
    #[allow(dead_code)]
    Satisfiable { best_solution: Solution },
    /// No solutions exist to the constraint satisfaction problem.
    Infeasible,
    /// No solution was found within the time budget.
    Unknown,
}
