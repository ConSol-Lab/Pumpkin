use log::debug;
use pumpkin_core::conflict_resolving::ConflictResolver;
use pumpkin_solver::Solver;
use pumpkin_solver::core::Function;
use pumpkin_solver::core::branching::Brancher;
use pumpkin_solver::core::results::SatisfactionResult;
use pumpkin_solver::core::results::Solution;
use pumpkin_solver::core::termination::TerminationCondition;

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
        mut resolver: impl ConflictResolver,
    ) -> MaxSatOptimisationResult {
        let process_time = Stopwatch::starting_now();

        // Compute an initial solution from which to start minimizing
        let initial_solution_result = {
            let initial_solve_result =
                self.solver
                    .satisfy(&mut brancher, termination, &mut resolver);

            match initial_solve_result {
                SatisfactionResult::Satisfiable(satisfiable) => {
                    debug!(
                        "Initial solution took {} seconds",
                        process_time.elapsed().as_secs(),
                    );

                    InitialSolveResult::Solution(satisfiable.solution().into())
                }
                SatisfactionResult::Unsatisfiable(_, _, _) => InitialSolveResult::Unsatisfiable,
                SatisfactionResult::Unknown(_, _, _) => InitialSolveResult::Unknown,
            }
        };

        match initial_solution_result {
            InitialSolveResult::Solution(solution) => self.linear_search.solve(
                &mut self.solver,
                process_time,
                &self.objective_function,
                termination,
                brancher,
                solution,
                &mut resolver,
            ),
            InitialSolveResult::Unsatisfiable => MaxSatOptimisationResult::Infeasible,
            InitialSolveResult::Unknown => MaxSatOptimisationResult::Unknown,
        }
    }
}

enum InitialSolveResult {
    Solution(Solution),
    Unsatisfiable,
    Unknown,
}
