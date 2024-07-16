use log::debug;
use pumpkin_lib::branching::Brancher;
use pumpkin_lib::results::SatisfactionResult;
use pumpkin_lib::termination::TerminationCondition;
use pumpkin_lib::Function;
use pumpkin_lib::Solver;

use super::linear_search::LinearSearch;
use super::optimisation_result::MaxSatOptimisationResult;
use super::stopwatch::Stopwatch;

/// Attempt to find optimal solutions to a constraint satisfaction problem with respect to an
/// objective function.
#[derive(Debug)]
pub(crate) struct OptimisationSolver {
    solver: Solver,
    objective_function: Function,
    linear_search: LinearSearch,
}

impl OptimisationSolver {
    pub(crate) fn new(
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
    pub(crate) fn solve(
        &mut self,
        termination: &mut impl TerminationCondition,
        mut brancher: impl Brancher,
    ) -> MaxSatOptimisationResult {
        let process_time = Stopwatch::starting_now();

        // Compute an initial solution from which to start minimizing
        let initial_solve_result = self.solver.satisfy(&mut brancher, termination);

        match initial_solve_result {
            SatisfactionResult::Satisfiable(initial_solution) => {
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
                    initial_solution,
                )
            }
            SatisfactionResult::Unsatisfiable => {
                self.solver.log_statistics();
                MaxSatOptimisationResult::Infeasible
            }
            SatisfactionResult::Unknown => {
                self.solver.log_statistics();
                MaxSatOptimisationResult::Unknown
            }
        }
    }
}
