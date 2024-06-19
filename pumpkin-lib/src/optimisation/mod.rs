mod linear_search;
mod optimiser;

pub use linear_search::*;
use log::debug;
pub use optimiser::*;

use crate::basic_types::statistic_logging::statistic_logger::log_statistic;
use crate::basic_types::statistic_logging::statistic_logger::log_statistic_postfix;
use crate::basic_types::CSPSolverExecutionFlag;
use crate::basic_types::Function;
use crate::basic_types::Stopwatch;
use crate::branching::Brancher;
use crate::engine::termination::TerminationCondition;
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

pub fn log_statistics_with_objective(
    csp_solver: &ConstraintSatisfactionSolver,
    best_objective_value: i64,
) {
    log_statistic("objective", best_objective_value);
    csp_solver.log_statistics();
    log_statistic_postfix();
}

pub fn log_statistics(csp_solver: &ConstraintSatisfactionSolver) {
    csp_solver.log_statistics();
    log_statistic_postfix();
}

impl OptimisationSolver {
    pub fn solve(
        &mut self,
        termination: &mut impl TerminationCondition,
        mut brancher: impl Brancher,
    ) -> OptimisationResult {
        let process_time = Stopwatch::starting_now();

        // Compute an initial solution from which to start minimizing
        let initial_solve_result = self.csp_solver.solve(termination, &mut brancher);

        match initial_solve_result {
            CSPSolverExecutionFlag::Infeasible => {
                log_statistics(&self.csp_solver);
                OptimisationResult::Infeasible
            }
            CSPSolverExecutionFlag::Timeout => {
                log_statistics(&self.csp_solver);
                OptimisationResult::Unknown
            }
            _ => {
                debug!(
                    "Initial solution took {} seconds",
                    process_time.elapsed().as_secs(),
                );

                self.linear_search.solve(
                    &mut self.csp_solver,
                    process_time,
                    &self.objective_function,
                    termination,
                    brancher,
                )
            }
        }
    }
}
