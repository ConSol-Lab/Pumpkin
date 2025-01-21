use log::info;

use super::OptimisationProcedure;
use crate::basic_types::CSPSolverExecutionFlag;
use crate::branching::Brancher;
use crate::predicate;
use crate::results::OptimisationResult;
use crate::results::Solution;
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

        // We keep track of the lower-bound on the variable
        let mut lower_bound = solver
            .satisfaction_solver
            .get_lower_bound(&objective_variable);

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

        loop {
            solver.satisfaction_solver.restore_state_at_root(brancher);

            // We know that the objective value should be at least the value which we are currently
            // attempting
            //
            // In the first iteration, this will add a trivial constraint
            //
            // In all subsequent iterations, if infeasibility is detected, then it will set the
            // lower-bound of the objective variable to be +1 of the last value attempted)
            let _ = solver.add_clause([predicate!(objective_variable >= lower_bound)]);

            // It could be the case that due to learning and/or propagation we have learned that
            // the lower-bound of the variable is already larger; we take this into account by
            // updating the variable
            lower_bound = lower_bound.max(
                solver
                    .satisfaction_solver
                    .get_lower_bound(&objective_variable),
            );

            info!(
                "LUS - Attempt to find solution with {}",
                predicate!(objective_variable <= lower_bound)
            );

            // Solve under the assumption that the objective variable is lower than `lower-bound`
            let solve_result = solver.satisfaction_solver.solve_under_assumptions(
                &[predicate!(objective_variable <= lower_bound)],
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
                    // The lower-bound is still too small, increase it until we find SAT
                    lower_bound += 1;
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
