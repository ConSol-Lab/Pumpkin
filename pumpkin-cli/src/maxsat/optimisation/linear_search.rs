use log::info;
use pumpkin_lib::asserts::pumpkin_assert_moderate;
use pumpkin_lib::branching::Brancher;
use pumpkin_lib::encodings::Function;
use pumpkin_lib::encodings::PseudoBooleanConstraintEncoder;
use pumpkin_lib::encodings::PseudoBooleanEncoding;
use pumpkin_lib::results::ProblemSolution;
use pumpkin_lib::results::SatisfactionResult;
use pumpkin_lib::results::Solution;
use pumpkin_lib::solving::Solver;
use pumpkin_lib::termination::TerminationCondition;
use pumpkin_lib::variables::PropositionalVariable;

use super::optimisation_result::MaxSatOptimisationResult;
use super::stopwatch::Stopwatch;

#[derive(Debug, Copy, Clone)]
pub(crate) struct LinearSearch {
    upper_bound_encoding: PseudoBooleanEncoding,
}

impl LinearSearch {
    pub(crate) fn new(upper_bound_encoding: PseudoBooleanEncoding) -> LinearSearch {
        LinearSearch {
            upper_bound_encoding,
        }
    }

    pub(crate) fn solve(
        &self,
        solver: &mut Solver,
        process_time: Stopwatch,
        objective_function: &Function,
        termination: &mut impl TerminationCondition,
        mut brancher: impl Brancher,
    ) -> MaxSatOptimisationResult {
        let mut best_solution: Solution = solver.get_solution_reference().into();

        let mut best_objective_value =
            objective_function.evaluate_assignment(best_solution.as_reference());

        println!("o {}", best_objective_value);
        info!(
            "Current objective is {} after {} seconds ({} ms)",
            best_objective_value,
            process_time.elapsed().as_secs(),
            process_time.elapsed().as_millis(),
        );
        let mut upper_bound_encoder = PseudoBooleanConstraintEncoder::from_function(
            objective_function,
            solver,
            self.upper_bound_encoding,
        );

        let mut first_iteration = true;

        loop {
            if best_objective_value == objective_function.get_constant_term() {
                solver.log_statistics_with_objective(best_objective_value as i64);
                return MaxSatOptimisationResult::Optimal {
                    solution: best_solution,
                };
            }

            solver.restore_state_at_root(&mut brancher);

            let encoding_status =
                upper_bound_encoder.constrain_at_most_k(best_objective_value - 1, solver);

            if first_iteration {
                brancher.on_encoding_objective_function(
                    &(1..solver
                        .get_solution_reference()
                        .num_propositional_variables() as u32)
                        .map(PropositionalVariable::new)
                        .collect::<Vec<_>>(),
                );

                first_iteration = false;
            }

            // in case some cases infeasibility can be detected while constraining the upper bound
            //  meaning the current best solution is optimal
            if encoding_status.is_err() {
                solver.log_statistics_with_objective(best_objective_value as i64);
                return MaxSatOptimisationResult::Optimal {
                    solution: best_solution,
                };
            }

            let result = solver.satisfy(&mut brancher, termination);

            match result {
                SatisfactionResult::Satisfiable(satisfiable) => {
                    let solution_ref = satisfiable.as_solution();
                    let new_objective_value = objective_function.evaluate_assignment(solution_ref);

                    pumpkin_assert_moderate!(
                        new_objective_value < best_objective_value,
                        "Each iteration of linear search must yield a strictly better solution."
                    );

                    // need to include a simple refinement step here, since it could be that the
                    // returned solution can be trivially improved

                    best_objective_value = new_objective_value;
                    best_solution = solution_ref.into();

                    println!("o {}", best_objective_value);
                    info!(
                        "Current objective is {} after {} seconds ({} ms)",
                        best_objective_value,
                        process_time.elapsed().as_secs(),
                        process_time.elapsed().as_millis(),
                    );
                }
                SatisfactionResult::Unsatisfiable => {
                    solver.log_statistics_with_objective(best_objective_value as i64);

                    return MaxSatOptimisationResult::Optimal {
                        solution: best_solution,
                    };
                }
                SatisfactionResult::Unknown => {
                    solver.log_statistics_with_objective(best_objective_value as i64);
                    return MaxSatOptimisationResult::Satisfiable { best_solution };
                }
            }
        }
    }
}
