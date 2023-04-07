use crate::{
    basic_types::{CSPSolverExecutionFlag, Function, Solution, SolutionValuePair, Stopwatch},
    encoders::{
        CardinalityNetworkEncoder, EncodingStatus, GeneralisedTotaliserEncoder, UpperBoundEncoder,
    },
    pumpkin_asserts::{pumpkin_assert_moderate, pumpkin_assert_simple},
};
use log::info;

use super::ConstraintSatisfactionSolver;

pub struct LinearSearch {
    upper_bound_encoding: UpperBoundEncoding,
}

#[derive(Clone, Copy, Debug)]
#[allow(clippy::upper_case_acronyms)]
pub enum UpperBoundEncoding {
    GTE,
    CNE,
}

impl LinearSearch {
    pub fn new(upper_bound_encoding: UpperBoundEncoding) -> LinearSearch {
        LinearSearch {
            upper_bound_encoding,
        }
    }

    pub fn solve(
        &self,
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
        info!(
            "Current objective is {} after {} seconds",
            best_objective_value,
            stopwatch.get_elapsed_time()
        );

        let mut upper_bound_encoder: Box<dyn UpperBoundEncoder> = match self.upper_bound_encoding {
            UpperBoundEncoding::GTE => Box::new(GeneralisedTotaliserEncoder::new(
                objective_function,
                csp_solver,
            )),
            UpperBoundEncoding::CNE => Box::new(CardinalityNetworkEncoder::from_function(
                objective_function,
                csp_solver,
            )),
        };

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
                    info!("Current objective is {} after {} seconds", best_objective_value, stopwatch.get_elapsed_time());
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
