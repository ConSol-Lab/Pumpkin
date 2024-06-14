use std::time::Duration;

use pumpkin_lib::basic_types::CSPSolverExecutionFlag;
use pumpkin_lib::basic_types::ConstraintOperationError;
use pumpkin_lib::basic_types::Stopwatch;
use pumpkin_lib::branching::Brancher;
use pumpkin_lib::branching::DynamicBrancher;
use pumpkin_lib::engine::ConstraintSatisfactionSolver;
use pumpkin_lib::optimisation::log_statistics_with_objective;
use pumpkin_lib::pumpkin_assert_simple;

use super::instance::FlatzincObjective;
use super::instance::Output;
use super::print_solution_from_solver;

pub(crate) struct MinizincOptimiser<'a> {
    csp_solver: &'a mut ConstraintSatisfactionSolver,
    objective_function: FlatzincObjective,
}

impl<'a> MinizincOptimiser<'a> {
    pub(crate) fn new(
        csp_solver: &'a mut ConstraintSatisfactionSolver,
        objective_function: FlatzincObjective,
    ) -> Self {
        Self {
            csp_solver,
            objective_function,
        }
    }

    pub(crate) fn solve(
        &mut self,
        time_limit: Option<Duration>,
        mut brancher: DynamicBrancher,
        outputs: &[Output],
    ) -> MinizincOptimisationResult {
        let stopwatch = Stopwatch::new(
            time_limit
                .map(|limit| limit.as_secs() as i64)
                .unwrap_or(i64::MAX),
        );

        let initial_solve = self
            .csp_solver
            .solve(stopwatch.get_remaining_time_budget(), &mut brancher);
        match initial_solve {
            CSPSolverExecutionFlag::Feasible => {
                #[allow(deprecated)]
                brancher.on_solution(self.csp_solver.get_solution_reference());

                log_statistics_with_objective(
                    self.csp_solver,
                    self.csp_solver
                        .get_assigned_integer_value(self.objective_function.get_domain())
                        .expect("expected variable to be assigned") as i64,
                );
                #[allow(deprecated)]
                print_solution_from_solver(self.csp_solver.get_solution_reference(), outputs)
            }
            CSPSolverExecutionFlag::Infeasible => return MinizincOptimisationResult::Infeasible,
            CSPSolverExecutionFlag::Timeout => return MinizincOptimisationResult::Unknown,
        }

        let mut best_objective_value =
            self.csp_solver
                .get_assigned_integer_value(self.objective_function.get_domain())
                .expect("expected variable to be assigned") as i64;

        loop {
            self.csp_solver.restore_state_at_root(&mut brancher);

            if self.strengthen(best_objective_value).is_err() {
                return MinizincOptimisationResult::Optimal {
                    optimal_objective_value: best_objective_value,
                };
            }

            let solve_result = self
                .csp_solver
                .solve(stopwatch.get_remaining_time_budget(), &mut brancher);
            match solve_result {
                CSPSolverExecutionFlag::Feasible => {
                    self.debug_bound_change(best_objective_value);

                    best_objective_value = self
                        .csp_solver
                        .get_assigned_integer_value(self.objective_function.get_domain())
                        .expect("expected variable to be assigned")
                        as i64;

                    #[allow(deprecated)]
                    brancher.on_solution(self.csp_solver.get_solution_reference());

                    log_statistics_with_objective(
                        self.csp_solver,
                        self.csp_solver
                            .get_assigned_integer_value(self.objective_function.get_domain())
                            .expect("expected variable to be assigned")
                            as i64,
                    );
                    #[allow(deprecated)]
                    print_solution_from_solver(self.csp_solver.get_solution_reference(), outputs)
                }
                CSPSolverExecutionFlag::Infeasible => {
                    return MinizincOptimisationResult::Optimal {
                        optimal_objective_value: best_objective_value,
                    }
                }
                CSPSolverExecutionFlag::Timeout => {
                    return MinizincOptimisationResult::Satisfiable {
                        best_found_objective_value: best_objective_value,
                    }
                }
            }
        }
    }

    fn strengthen(&mut self, best_objective_value: i64) -> Result<(), ConstraintOperationError> {
        match self.objective_function {
            FlatzincObjective::Maximize(domain) => self.csp_solver.add_clause([self
                .csp_solver
                .get_lower_bound_literal(domain, (best_objective_value + 1) as i32)]),
            FlatzincObjective::Minimize(domain) => self.csp_solver.add_clause([self
                .csp_solver
                .get_upper_bound_literal(domain, (best_objective_value - 1) as i32)]),
        }
    }

    fn debug_bound_change(&self, best_objective_value: i64) {
        pumpkin_assert_simple!(
            match self.objective_function {
                FlatzincObjective::Maximize(_) => {
                    (self
                        .csp_solver
                        .get_assigned_integer_value(self.objective_function.get_domain())
                        .expect("expected variable to be assigned") as i64)
                        > best_objective_value
                }
                FlatzincObjective::Minimize(_) => {
                    (self
                        .csp_solver
                        .get_assigned_integer_value(self.objective_function.get_domain())
                        .expect("expected variable to be assigned") as i64)
                        < best_objective_value
                }
            },
            "{}",
            match self.objective_function {
                FlatzincObjective::Maximize(_) => format!(
                    "The current bound {} should be larger than the previous bound {}",
                    self.csp_solver
                        .get_assigned_integer_value(self.objective_function.get_domain())
                        .expect("expected variable to be assigned"),
                    best_objective_value
                ),
                FlatzincObjective::Minimize(_) => format!(
                    "The current bound {} should be smaller than the previous bound {}",
                    self.csp_solver
                        .get_assigned_integer_value(self.objective_function.get_domain())
                        .expect("expected variable to be assigned"),
                    best_objective_value
                ),
            }
        );
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
