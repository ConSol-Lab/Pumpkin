mod linear_search;
mod optimiser;

use std::time::Duration;

pub use linear_search::*;
use log::debug;
pub use optimiser::*;

use crate::basic_types::CSPSolverExecutionFlag;
use crate::basic_types::Function;
use crate::basic_types::Stopwatch;
use crate::branching::Brancher;
use crate::engine::ConstraintSatisfactionSolver;

/// Attempt to find optimal solutions to a constraint satisfaction problem with respect to an
/// objective function.
#[derive(Debug)]
pub struct OptimisationSolver {
    csp_solver: ConstraintSatisfactionSolver,
    objective_function: Function,
    linear_search: LinearSearch,
}

impl OptimisationSolver {
    pub fn new(
        csp_solver: ConstraintSatisfactionSolver,
        objective_function: Function,
        linear_search: LinearSearch,
    ) -> OptimisationSolver {
        OptimisationSolver {
            csp_solver,
            objective_function,
            linear_search,
        }
    }
}

impl OptimisationSolver {
    pub fn solve(
        &mut self,
        time_limit: Option<Duration>,
        mut brancher: impl Brancher,
    ) -> OptimisationResult {
        let stopwatch = Stopwatch::new(
            time_limit
                .map(|limit| limit.as_secs() as i64)
                .unwrap_or(i64::MAX),
        );

        // Compute an initial solution from which to start minimizing
        let initial_solve_result = self
            .csp_solver
            .solve(stopwatch.get_remaining_time_budget(), &mut brancher);

        match initial_solve_result {
            CSPSolverExecutionFlag::Infeasible => {
                self.csp_solver.log_statistics();
                OptimisationResult::Infeasible
            }
            CSPSolverExecutionFlag::Timeout => {
                self.csp_solver.log_statistics();
                OptimisationResult::Unknown
            }
            _ => {
                debug!(
                    "Initial solution took {} seconds",
                    stopwatch.get_elapsed_time()
                );

                self.linear_search.solve(
                    &mut self.csp_solver,
                    &self.objective_function,
                    &stopwatch,
                    brancher,
                )
            }
        }
    }
}
