use pumpkin_lib::branching::branchers::dynamic_brancher::DynamicBrancher;
use pumpkin_lib::branching::Brancher;
use pumpkin_lib::predicate;
use pumpkin_lib::pumpkin_assert_simple;
use pumpkin_lib::results::OptimisationResult;
use pumpkin_lib::results::ProblemSolution;
use pumpkin_lib::termination::TerminationCondition;
use pumpkin_lib::Solver;

use super::instance::FlatzincObjective;
use super::instance::Output;
use super::print_solution_from_solver;

pub(crate) struct MinizincOptimiser<'a> {
    solver: &'a mut Solver,
    objective_function: FlatzincObjective,
}

impl<'a> MinizincOptimiser<'a> {
    pub(crate) fn new(csp_solver: &'a mut Solver, objective_function: FlatzincObjective) -> Self {
        Self {
            solver: csp_solver,
            objective_function,
        }
    }

    pub(crate) fn solve(
        &mut self,
        termination: &mut impl TerminationCondition,
        mut brancher: DynamicBrancher,
        outputs: &[Output],
    ) -> MinizincOptimisationResult {
        let output = match self.objective_function {
            FlatzincObjective::Maximize(objective_variable) => {
                self.solver
                    .maximise(&mut brancher, termination, objective_variable)
            }
            FlatzincObjective::Minimize(objective_variable) => {
                self.solver
                    .minimise(&mut brancher, termination, objective_variable)
            }
        };
        match output {
            OptimisationResult::Optimal(solution) => MinizincOptimisationResult::Optimal {
                optimal_objective_value: solution
                    .get_integer_value(*self.objective_function.get_domain())
                    as i64,
            },
            OptimisationResult::Satisfiable(solution) => MinizincOptimisationResult::Satisfiable {
                best_found_objective_value: solution
                    .get_integer_value(*self.objective_function.get_domain())
                    as i64,
            },
            OptimisationResult::Unsatisfiable => MinizincOptimisationResult::Infeasible,
            OptimisationResult::Unknown => MinizincOptimisationResult::Unknown,
        }
    }
}

/// The result of calling [`MinizincOptimiser::solve()`].
#[derive(Debug)]
pub(crate) enum MinizincOptimisationResult {
    /// There exists no solution with a better objective value than this one.
    Optimal { optimal_objective_value: i64 },
    /// The optimal solution was not found within the time budget. However, at least one solution
    /// was found. The provided solution is the solution with the best objective value that was
    /// encountered.
    Satisfiable { best_found_objective_value: i64 },
    /// No solutions exist to the constraint satisfaction problem.
    Infeasible,
    /// No solution was found within the time budget.
    Unknown,
}
