use crate::{
    basic_types::{CSPSolverExecutionFlag, Function, Solution, SolutionValuePair, Stopwatch},
    encoders::{EncodingStatus, GeneralisedTotaliserEncoder},
    pumpkin_asserts::{pumpkin_assert_moderate, pumpkin_assert_simple},
};

use super::ConstraintSatisfactionSolver;

pub struct LinearSearch {}

impl LinearSearch {
    pub fn new() -> LinearSearch {
        LinearSearch {}
    }

    pub fn solve(
        csp_solver: &mut ConstraintSatisfactionSolver,
        objective_function: &Function,
        stopwatch: &Stopwatch,
    ) -> SolutionValuePair {
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
        println!("c t = {} s", stopwatch.get_elapsed_time());

        let mut upper_bound_encoder =
            GeneralisedTotaliserEncoder::new(objective_function, csp_solver);

        loop {
            if best_objective_value == objective_function.get_constant_term() {
                return SolutionValuePair::new(best_solution, best_objective_value);
            }

            csp_solver.set_solution_guided_search();

            csp_solver.restore_state_at_root();

            let encoding_status =
                upper_bound_encoder.constrain_at_most_k(best_objective_value - 1, csp_solver);

            //in case some cases infeasibility can be detected while constraining the upper bound
            //  meaning the current best solution is optimal
            if let EncodingStatus::ConflictDetected = encoding_status {
                return SolutionValuePair::new(best_solution, best_objective_value);
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
                    println!("c t = {} s", stopwatch.get_elapsed_time());
                }
                CSPSolverExecutionFlag::Infeasible => {
                    return SolutionValuePair::new(best_solution, best_objective_value);
                }
                CSPSolverExecutionFlag::Timeout => {
                    return SolutionValuePair::new(best_solution, best_objective_value);
                }
                CSPSolverExecutionFlag::InfeasibleUnderAssumptions => panic!("Do not expect to be infeasible under assumption as a result of a linear search call.")               
            }
        }
    }
}
