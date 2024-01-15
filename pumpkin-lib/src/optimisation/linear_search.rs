use log::info;

use super::OptimisationResult;
use crate::basic_types::CSPSolverExecutionFlag;
use crate::basic_types::Function;
use crate::basic_types::Solution;
use crate::basic_types::Stopwatch;
use crate::encoders::PseudoBooleanConstraintEncoder;
use crate::encoders::PseudoBooleanEncoding;
use crate::engine::ConstraintSatisfactionSolver;
use crate::pumpkin_assert_moderate;
use crate::pumpkin_assert_simple;

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
            "Current objective is {} after {} seconds",
            best_objective_value,
            stopwatch.get_elapsed_time()
        );

        let mut upper_bound_encoder = PseudoBooleanConstraintEncoder::from_function(
            objective_function,
            csp_solver,
            self.upper_bound_encoding,
        );

        loop {
            if best_objective_value == upper_bound_encoder.get_constant_term() {
                return OptimisationResult::Optimal {
                    solution: best_solution,
                    objective_value: best_objective_value,
                };
            }

            csp_solver.set_solution_guided_search();

            csp_solver.restore_state_at_root();

            let encoding_status =
                upper_bound_encoder.constrain_at_most_k(best_objective_value - 1, csp_solver);

            //in case some cases infeasibility can be detected while constraining the upper bound
            //  meaning the current best solution is optimal
            if encoding_status.is_err() {
                return OptimisationResult::Optimal {
                    solution: best_solution,
                    objective_value: best_objective_value,
                };
            }

            let csp_execution_flag = csp_solver.solve(stopwatch.get_remaining_time_budget());

            match csp_execution_flag {
                CSPSolverExecutionFlag::Feasible => {
                    pumpkin_assert_moderate!(
                        objective_function.evaluate_assignment(
                            csp_solver.get_propositional_assignments(),
                            csp_solver.get_integer_assignments()
                        ) < best_objective_value,
                        "Each iteration of linear search must yield a strictly better solution."
                    );

                    //need to include a simple refinement step here, since it could be that the returned solution can be trivially improved

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
                        "Current objective is {} after {} seconds",
                        best_objective_value,
                        stopwatch.get_elapsed_time()
                    );
                }
                CSPSolverExecutionFlag::Infeasible => {
                    return OptimisationResult::Optimal {
                        solution: best_solution,
                        objective_value: best_objective_value,
                    };
                }
                CSPSolverExecutionFlag::Timeout => {
                    return OptimisationResult::Satisfiable {
                        best_solution,
                        objective_value: best_objective_value,
                    };
                }
            }
        }
    }
}
