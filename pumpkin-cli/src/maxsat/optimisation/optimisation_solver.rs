use log::debug;
use pumpkin_lib::{
    branching::Brancher, statistics::log_statistics, termination::TerminationCondition, Function,
    Solver, Stopwatch,
};

use super::{linear_search::LinearSearch, optimisation_result::OptimisationResult};

/// Attempt to find optimal solutions to a constraint satisfaction problem with respect to an
/// objective function.
#[derive(Debug)]
pub struct OptimisationSolver {
    solver: Solver,
    objective_function: Function,
    linear_search: LinearSearch,
}

impl OptimisationSolver {
    pub fn new(
        csp_solver: Solver,
        objective_function: Function,
        linear_search: LinearSearch,
    ) -> OptimisationSolver {
        OptimisationSolver {
            solver: csp_solver,
            objective_function,
            linear_search,
        }
    }
}

impl OptimisationSolver {
    pub fn solve(
        &mut self,
        termination: &mut impl TerminationCondition,
        mut brancher: impl Brancher,
    ) -> OptimisationResult {
        let process_time = Stopwatch::starting_now();

        // Compute an initial solution from which to start minimizing
        let initial_solve_result = self.solver.satisfy(&mut brancher, termination);

        match initial_solve_result {
            pumpkin_lib::results::SatisfactionResult::Satisfiable(_) => {
                debug!(
                    "Initial solution took {} seconds",
                    process_time.elapsed().as_secs(),
                );

                self.linear_search.solve(
                    &mut self.solver,
                    process_time,
                    &self.objective_function,
                    termination,
                    brancher,
                )
            }
            pumpkin_lib::results::SatisfactionResult::Unsatisfiable => {
                self.solver.log_statistics();
                OptimisationResult::Infeasible
            }
            pumpkin_lib::results::SatisfactionResult::Unknown => {
                self.solver.log_statistics();
                OptimisationResult::Unknown
            }
        }
    }
}
