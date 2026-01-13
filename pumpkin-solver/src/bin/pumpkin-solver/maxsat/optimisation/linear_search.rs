use log::info;
use pumpkin_core::conflict_resolving::ConflictResolver;
use pumpkin_solver::Function;
use pumpkin_solver::Solver;
use pumpkin_solver::asserts::pumpkin_assert_moderate;
use pumpkin_solver::branching::Brancher;
use pumpkin_solver::results::SatisfactionResult;
use pumpkin_solver::results::Solution;
use pumpkin_solver::termination::TerminationCondition;

use super::optimisation_result::MaxSatOptimisationResult;
use super::stopwatch::Stopwatch;
use crate::maxsat::encoders::PseudoBooleanConstraintEncoder;
use crate::maxsat::encoders::PseudoBooleanEncoding;

#[derive(Debug, Copy, Clone)]
pub(crate) struct LinearSearch {
    encoding: PseudoBooleanEncoding,
}

impl LinearSearch {
    pub(crate) fn new(encoding: PseudoBooleanEncoding) -> LinearSearch {
        LinearSearch { encoding }
    }

    #[allow(clippy::too_many_arguments, reason = "Should be refactored")]
    pub(crate) fn solve(
        &self,
        solver: &mut Solver,
        process_time: Stopwatch,
        objective_function: &Function,
        termination: &mut impl TerminationCondition,
        mut brancher: impl Brancher,
        initial_solution: Solution,
        resolver: &mut impl ConflictResolver,
    ) -> MaxSatOptimisationResult {
        brancher.on_solution(initial_solution.as_reference());

        let mut best_solution: Solution = initial_solution;
        let mut best_objective_value =
            objective_function.evaluate_assignment(best_solution.as_reference());

        solver.log_statistics_with_objective(Some(&brancher), best_objective_value as i64, true);
        println!("o {best_objective_value}");
        info!(
            "Current objective is {} after {} seconds ({} ms)",
            best_objective_value,
            process_time.elapsed().as_secs(),
            process_time.elapsed().as_millis(),
        );
        let mut upper_bound_encoder = PseudoBooleanConstraintEncoder::from_function(
            objective_function,
            solver,
            self.encoding,
        );

        loop {
            if best_objective_value == objective_function.get_constant_term() {
                solver.log_statistics_with_objective(
                    Some(&brancher),
                    best_objective_value as i64,
                    true,
                );
                return MaxSatOptimisationResult::Optimal {
                    solution: best_solution,
                };
            }

            let encoding_status =
                upper_bound_encoder.constrain_at_most_k(best_objective_value - 1, solver);

            // in case some cases infeasibility can be detected while constraining the upper bound
            //  meaning the current best solution is optimal
            if encoding_status.is_err() {
                solver.log_statistics_with_objective(
                    Some(&brancher),
                    best_objective_value as i64,
                    true,
                );
                return MaxSatOptimisationResult::Optimal {
                    solution: best_solution,
                };
            }

            let result = solver.satisfy(&mut brancher, termination, resolver);

            match result {
                SatisfactionResult::Satisfiable(satisfiable) => {
                    let new_objective_value =
                        objective_function.evaluate_assignment(satisfiable.solution());

                    pumpkin_assert_moderate!(
                        new_objective_value < best_objective_value,
                        "Each iteration of linear search must yield a strictly better solution."
                    );

                    // need to include a simple refinement step here, since it could be that the
                    // returned solution can be trivially improved

                    best_objective_value = new_objective_value;
                    best_solution = satisfiable.solution().into();

                    satisfiable.solver().log_statistics_with_objective(
                        Some(satisfiable.brancher()),
                        best_objective_value as i64,
                        true,
                    );

                    println!("o {best_objective_value}");
                    info!(
                        "Current objective is {} after {} seconds ({} ms)",
                        best_objective_value,
                        process_time.elapsed().as_secs(),
                        process_time.elapsed().as_millis(),
                    );
                }
                SatisfactionResult::Unsatisfiable(solver, brancher) => {
                    solver.log_statistics_with_objective(
                        Some(brancher),
                        best_objective_value as i64,
                        true,
                    );

                    return MaxSatOptimisationResult::Optimal {
                        solution: best_solution,
                    };
                }
                SatisfactionResult::Unknown(solver, brancher) => {
                    solver.log_statistics_with_objective(
                        Some(brancher),
                        best_objective_value as i64,
                        true,
                    );
                    return MaxSatOptimisationResult::Satisfiable { best_solution };
                }
            }
        }
    }
}
