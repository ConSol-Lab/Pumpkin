use log::info;

use super::OptimisationResult;
use crate::basic_types::CSPSolverExecutionFlag;
use crate::basic_types::Function;
use crate::basic_types::Solution;
use crate::basic_types::Stopwatch;
use crate::branching::Brancher;
use crate::encoders::PseudoBooleanConstraintEncoder;
use crate::encoders::PseudoBooleanEncoding;
use crate::engine::ConstraintSatisfactionSolver;
use crate::pumpkin_assert_moderate;
use crate::pumpkin_assert_simple;

#[derive(Debug, Copy, Clone)]
pub struct LinearSearch {
    upper_bound_encoding: PseudoBooleanEncoding,
}

impl LinearSearch {
    pub fn new(upper_bound_encoding: PseudoBooleanEncoding) -> LinearSearch {
        LinearSearch {
            upper_bound_encoding,
        }
    }

    pub fn solve(
        &self,
        csp_solver: &mut ConstraintSatisfactionSolver,
        objective_function: &Function,
        stopwatch: &Stopwatch,
        mut brancher: impl Brancher,
    ) -> OptimisationResult {
        pumpkin_assert_simple!(
            csp_solver.get_state().has_solution(),
            "Linear search assumes the solver contains a feasible solution."
        );

        let mut best_solution = Solution::new(
            csp_solver.get_propositional_assignments(),
            csp_solver.get_integer_assignments(),
        );

        let mut best_objective_value = objective_function.evaluate_assignment(
            csp_solver.get_propositional_assignments(),
            csp_solver.get_integer_assignments(),
        );

        println!("o {}", best_objective_value);
        info!(
            "Current objective is {} after {} seconds ({} ms)",
            best_objective_value,
            stopwatch.get_elapsed_time(),
            stopwatch.get_elapsed_time_millis(),
        );
        let mut upper_bound_encoder = PseudoBooleanConstraintEncoder::from_function(
            objective_function,
            csp_solver,
            self.upper_bound_encoding,
        );

        let mut first_iteration = true;

        loop {
            if best_objective_value == objective_function.get_constant_term() {
                csp_solver.log_statistics();
                return OptimisationResult::Optimal {
                    solution: best_solution.clone(),
                    objective_value: best_objective_value as i64,
                };
            }

            csp_solver.restore_state_at_root(&mut brancher);

            let encoding_status =
                upper_bound_encoder.constrain_at_most_k(best_objective_value - 1, csp_solver);

            if first_iteration {
                brancher.on_encoding_objective_function(
                    &csp_solver
                        .get_propositional_assignments()
                        .get_propositional_variables()
                        .collect::<Vec<_>>(),
                );

                first_iteration = false;
            }

            // in case some cases infeasibility can be detected while constraining the upper bound
            //  meaning the current best solution is optimal
            if encoding_status.is_err() {
                csp_solver.log_statistics();
                return OptimisationResult::Optimal {
                    solution: best_solution,
                    objective_value: best_objective_value as i64,
                };
            }

            brancher.on_solution(&best_solution);

            let csp_execution_flag =
                csp_solver.solve(stopwatch.get_remaining_time_budget(), &mut brancher);

            match csp_execution_flag {
                CSPSolverExecutionFlag::Feasible => {
                    pumpkin_assert_moderate!(
                        objective_function.evaluate_assignment(
                            csp_solver.get_propositional_assignments(),
                            csp_solver.get_integer_assignments()
                        ) < best_objective_value,
                        "Each iteration of linear search must yield a strictly better solution."
                    );

                    // need to include a simple refinement step here, since it could be that the
                    // returned solution can be trivially improved

                    best_objective_value = objective_function.evaluate_assignment(
                        csp_solver.get_propositional_assignments(),
                        csp_solver.get_integer_assignments(),
                    );
                    best_solution.update(
                        csp_solver.get_propositional_assignments(),
                        csp_solver.get_integer_assignments(),
                    );

                    println!("o {}", best_objective_value);
                    info!(
                        "Current objective is {} after {} seconds ({} ms)",
                        best_objective_value,
                        stopwatch.get_elapsed_time(),
                        stopwatch.get_elapsed_time_millis(),
                    );
                }
                CSPSolverExecutionFlag::Infeasible => {
                    csp_solver.log_statistics();
                    return OptimisationResult::Optimal {
                        solution: best_solution,
                        objective_value: best_objective_value as i64,
                    };
                }
                CSPSolverExecutionFlag::Timeout => {
                    csp_solver.log_statistics();
                    return OptimisationResult::Satisfiable {
                        best_solution,
                        objective_value: best_objective_value as i64,
                    };
                }
            }
        }
    }
}
