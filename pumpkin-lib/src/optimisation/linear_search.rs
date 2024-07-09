use log::info;

use super::OptimisationResult;
use crate::basic_types::CSPSolverExecutionFlag;
use crate::basic_types::Function;
use crate::basic_types::Solution;
use crate::basic_types::Stopwatch;
use crate::branching::Brancher;
use crate::engine::predicates::integer_predicate::IntegerPredicate;
use crate::engine::termination::TerminationCondition;
use crate::engine::ConstraintSatisfactionSolver;
use crate::optimisation::log_statistics_with_objective;
use crate::pumpkin_assert_moderate;
use crate::pumpkin_assert_simple;

#[derive(Debug, Copy, Clone, Default)]
pub struct LinearSearch {}

impl LinearSearch {
    pub fn new() -> LinearSearch {
        LinearSearch {}
    }

    pub fn solve(
        &self,
        csp_solver: &mut ConstraintSatisfactionSolver,
        process_time: Stopwatch,
        objective_function: &Function,
        termination: &mut impl TerminationCondition,
        mut brancher: impl Brancher,
    ) -> OptimisationResult {
        pumpkin_assert_simple!(
            csp_solver.get_state().has_solution(),
            "Linear search assumes the solver contains a feasible solution."
        );

        pumpkin_assert_simple!(
            objective_function.get_terms().len() == 1,
            "Currently we assume the objective is composed of a single integer."
        );

        #[allow(deprecated)]
        let mut best_solution: Solution = csp_solver.get_solution_reference().into();

        let mut best_objective_value =
            objective_function.evaluate_assignment(best_solution.as_reference());

        println!("o {}", best_objective_value);
        info!(
            "Current objective is {} after {} seconds ({} ms)",
            best_objective_value,
            process_time.elapsed().as_secs(),
            process_time.elapsed().as_millis(),
        );

        loop {
            if best_objective_value == objective_function.get_constant_term() {
                log_statistics_with_objective(csp_solver, best_objective_value as i64);
                return OptimisationResult::Optimal {
                    solution: best_solution,
                    objective_value: best_objective_value as i64,
                };
            }

            // Record the internal value of the objective.
            // Important to do this before restoring at the root.
            assert!(objective_function.get_terms().len() == 1);
            let objective_variable = *objective_function.get_terms().next().unwrap().0;
            let internal_objective_value = csp_solver
                .get_assigned_integer_value(&objective_variable)
                .expect("The objective must be assigned.");

            csp_solver.restore_state_at_root(&mut brancher);

            // Add constraint on the upper bound of the objective function.
            // todo: check if this is okay, or whether it should go through the view!
            let objective_predicate = IntegerPredicate::UpperBound {
                domain_id: objective_variable,
                upper_bound: internal_objective_value - 1,
            };
            let obj_status = csp_solver
                .assignments_integer
                .post_integer_predicate(objective_predicate, None);

            // In case some cases, infeasibility can be detected while constraining the upper bound,
            // meaning the current best solution is optimal.
            if obj_status.is_err() {
                log_statistics_with_objective(csp_solver, best_objective_value as i64);
                return OptimisationResult::Optimal {
                    solution: best_solution,
                    objective_value: best_objective_value as i64,
                };
            }

            // todo: double check this, it seems that it does not get the full solution,
            // but only root level values.
            #[allow(deprecated)]
            brancher.on_solution(csp_solver.get_solution_reference());

            let csp_execution_flag = csp_solver.solve(termination, &mut brancher);

            match csp_execution_flag {
                CSPSolverExecutionFlag::Feasible => {
                    #[allow(deprecated)]
                    let solution_ref = csp_solver.get_solution_reference();
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
                CSPSolverExecutionFlag::Infeasible => {
                    log_statistics_with_objective(csp_solver, best_objective_value as i64);

                    return OptimisationResult::Optimal {
                        solution: best_solution,
                        objective_value: best_objective_value as i64,
                    };
                }
                CSPSolverExecutionFlag::Timeout => {
                    log_statistics_with_objective(csp_solver, best_objective_value as i64);
                    return OptimisationResult::Satisfiable {
                        best_solution,
                        objective_value: best_objective_value as i64,
                    };
                }
            }
        }
    }
}
