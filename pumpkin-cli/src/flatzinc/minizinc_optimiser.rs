use std::time::Duration;

use pumpkin_lib::basic_types::CSPSolverExecutionFlag;
use pumpkin_lib::basic_types::Solution;
use pumpkin_lib::basic_types::Stopwatch;
use pumpkin_lib::engine::AssignmentsInteger;
use pumpkin_lib::engine::AssignmentsPropositional;
use pumpkin_lib::engine::ConstraintSatisfactionSolver;
use pumpkin_lib::optimisation::OptimisationResult;
use pumpkin_lib::pumpkin_assert_simple;

use super::instance::FlatZincInstance;
use super::instance::FlatzincObjective;
use super::print_solution_from_solver;

pub struct MinizincOptimiser<'a> {
    csp_solver: ConstraintSatisfactionSolver,
    objective_function: FlatzincObjective,
    instance: &'a FlatZincInstance,
}

impl<'a> MinizincOptimiser<'a> {
    pub fn new(
        csp_solver: ConstraintSatisfactionSolver,
        objective_function: FlatzincObjective,
        instance: &'a FlatZincInstance,
    ) -> Self {
        Self {
            csp_solver,
            objective_function,
            instance,
        }
    }

    pub fn solve(&mut self, time_limit: Option<Duration>) -> OptimisationResult {
        let stopwatch = Stopwatch::new(
            time_limit
                .map(|limit| limit.as_secs() as i64)
                .unwrap_or(i64::MAX),
        );

        let initial_solve = self.csp_solver.solve(stopwatch.get_remaining_time_budget());
        match initial_solve {
            CSPSolverExecutionFlag::Feasible => print_solution_from_solver(
                self.get_integer_assignments(),
                self.get_propositional_assignments(),
                self.instance,
            ),
            CSPSolverExecutionFlag::Infeasible => return OptimisationResult::Infeasible,
            CSPSolverExecutionFlag::Timeout => return OptimisationResult::Unknown,
        }

        let mut best_objective_value =
            self.csp_solver
                .get_integer_assignments()
                .get_assigned_value(*self.objective_function.get_domain()) as i64;
        let mut best_solution = Solution::new(
            self.csp_solver.get_propositional_assignments(),
            self.csp_solver.get_integer_assignments(),
        );

        loop {
            self.csp_solver.restore_state_at_root();

            if self.should_terminate(best_objective_value) {
                return OptimisationResult::Optimal {
                    solution: best_solution,
                    objective_value: best_objective_value,
                };
            } else {
                self.strengthen(best_objective_value);
            }

            let solve_result = self.csp_solver.solve(stopwatch.get_remaining_time_budget());
            match solve_result {
                CSPSolverExecutionFlag::Feasible => {
                    best_objective_value = self
                        .csp_solver
                        .get_integer_assignments()
                        .get_assigned_value(*self.objective_function.get_domain())
                        as i64;
                    best_solution.update(
                        self.csp_solver.get_propositional_assignments(),
                        self.csp_solver.get_integer_assignments(),
                    );
                    print_solution_from_solver(
                        self.get_integer_assignments(),
                        self.get_propositional_assignments(),
                        self.instance,
                    )
                }
                CSPSolverExecutionFlag::Infeasible => {
                    return OptimisationResult::Optimal {
                        solution: best_solution,
                        objective_value: best_objective_value,
                    }
                }
                CSPSolverExecutionFlag::Timeout => {
                    return OptimisationResult::Satisfiable {
                        best_solution,
                        objective_value: best_objective_value,
                    }
                }
            }
        }
    }

    fn should_terminate(&self, best_objective_value: i64) -> bool {
        pumpkin_assert_simple!(
            self.csp_solver.get_state().is_ready(),
            "Expected the solver to be in the root state"
        );
        match self.objective_function {
            FlatzincObjective::Maximize(domain) => {
                // The upper-bound might prevent us from strengthening it further
                self.csp_solver
                    .get_integer_assignments()
                    .get_upper_bound(domain) as i64
                    <= best_objective_value + 1
            }
            FlatzincObjective::Minimize(domain) => {
                // The lower-bound might prevent us from strengthening it further
                self.csp_solver
                    .get_integer_assignments()
                    .get_lower_bound(domain) as i64
                    >= best_objective_value - 1
            }
        }
    }

    fn strengthen(&mut self, best_objective_value: i64) {
        match self.objective_function {
            FlatzincObjective::Maximize(domain) => {
                let result = self.csp_solver.add_unit_clause(
                    self.csp_solver
                        .get_lower_bound_literal(domain, (best_objective_value + 1) as i32),
                );
                pumpkin_assert_simple!(result.is_ok())
            }
            FlatzincObjective::Minimize(domain) => {
                let result = self.csp_solver.add_unit_clause(
                    self.csp_solver
                        .get_upper_bound_literal(domain, (best_objective_value - 1) as i32),
                );
                pumpkin_assert_simple!(result.is_ok())
            }
        }
    }

    fn get_integer_assignments(&self) -> &AssignmentsInteger {
        self.csp_solver.get_integer_assignments()
    }

    fn get_propositional_assignments(&self) -> &AssignmentsPropositional {
        self.csp_solver.get_propositional_assignments()
    }
}