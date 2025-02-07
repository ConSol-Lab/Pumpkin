use super::OptimisationProcedure;
use crate::basic_types::CSPSolverExecutionFlag;
use crate::branching::Brancher;
use crate::predicate;
use crate::pumpkin_assert_simple;
use crate::results::OptimisationResult;
use crate::results::Solution;
use crate::results::SolutionCallbackArguments;
use crate::termination::TerminationCondition;
use crate::variables::IntegerVariable;
use crate::Solver;

#[derive(Debug, Clone, Copy)]
pub struct LowerBoundingSearch;

impl OptimisationProcedure for LowerBoundingSearch {
    fn minimise(
        &mut self,
        brancher: &mut impl Brancher,
        termination: &mut impl TerminationCondition,
        objective_variable: impl IntegerVariable,
        is_maximising: bool,
        solver: &mut Solver,
    ) -> OptimisationResult {
        // If we are maximising then when we simply scale the variable by -1, however, this will
        // lead to the printed objective value in the statistics to be multiplied by -1; this
        // objective_multiplier ensures that the objective is correctly logged.
        let objective_multiplier = if is_maximising { -1 } else { 1 };

        // First we do a feasibility check
        let feasibility_check = solver.satisfaction_solver.solve(termination, brancher);
        match feasibility_check {
            CSPSolverExecutionFlag::Feasible => {}
            CSPSolverExecutionFlag::Infeasible => {
                // Reset the state whenever we return a result
                solver.satisfaction_solver.restore_state_at_root(brancher);
                let _ = solver.satisfaction_solver.conclude_proof_unsat();
                return OptimisationResult::Unsatisfiable;
            }
            CSPSolverExecutionFlag::Timeout => {
                // Reset the state whenever we return a result
                solver.satisfaction_solver.restore_state_at_root(brancher);
                return OptimisationResult::Unknown;
            }
        }
        let mut best_objective_value = Default::default();
        let mut best_solution = Solution::default();

        self.update_best_solution_and_process(
            objective_multiplier,
            &objective_variable,
            &mut best_objective_value,
            &mut best_solution,
            brancher,
            solver,
        );
        solver.satisfaction_solver.restore_state_at_root(brancher);

        let result = solver.add_clause([predicate!(
            objective_variable <= best_objective_value as i32
        )]);
        pumpkin_assert_simple!(result.is_ok());

        loop {
            let assumption =
                predicate!(objective_variable <= solver.lower_bound(&objective_variable));

            // We check whether the lower-bound is now equal to the solution that we have found
            // previously
            if objective_multiplier * solver.lower_bound(&objective_variable)
                == best_objective_value as i32
            {
                // We create a predicate specifying the best-found solution for the proof
                // logging
                let objective_bound_predicate = if is_maximising {
                    predicate![
                        objective_variable >= best_objective_value as i32 * objective_multiplier
                    ]
                } else {
                    predicate![
                        objective_variable <= best_objective_value as i32 * objective_multiplier
                    ]
                };
                let _ = solver
                    .satisfaction_solver
                    .conclude_proof_optimal(objective_bound_predicate);

                return OptimisationResult::Optimal(best_solution);
            }

            println!(
                "Lower-Bounding Search - Attempting to find solution with assumption {assumption}"
            );

            // Solve under the assumption that the objective variable is lower than `lower-bound`
            let solve_result = solver.satisfaction_solver.solve_under_assumptions(
                &[assumption],
                termination,
                brancher,
            );
            match solve_result {
                CSPSolverExecutionFlag::Feasible => {
                    self.update_best_solution_and_process(
                        objective_multiplier,
                        &objective_variable,
                        &mut best_objective_value,
                        &mut best_solution,
                        brancher,
                        solver,
                    );

                    // Reset the state whenever we return a result
                    solver.satisfaction_solver.restore_state_at_root(brancher);

                    // We create a predicate specifying the best-found solution for the proof
                    // logging
                    let objective_bound_predicate = if is_maximising {
                        predicate![
                            objective_variable
                                >= best_objective_value as i32 * objective_multiplier
                        ]
                    } else {
                        predicate![
                            objective_variable
                                <= best_objective_value as i32 * objective_multiplier
                        ]
                    };
                    let _ = solver
                        .satisfaction_solver
                        .conclude_proof_optimal(objective_bound_predicate);

                    return OptimisationResult::Optimal(best_solution);
                }
                CSPSolverExecutionFlag::Infeasible => {
                    (solver.solution_callback)(SolutionCallbackArguments::new(
                        solver,
                        &best_solution,
                        Some(best_objective_value),
                    ));
                    solver.satisfaction_solver.restore_state_at_root(brancher);
                    // We add the (hard) constraint that the negated assumption should hold (i.e.,
                    // the solution should be at least as large as the found solution)
                    let _ = solver.add_clause([!assumption]);
                }
                CSPSolverExecutionFlag::Timeout => {
                    // Reset the state whenever we return a result
                    solver.satisfaction_solver.restore_state_at_root(brancher);
                    return OptimisationResult::Satisfiable(best_solution);
                }
            }
        }
    }
}
