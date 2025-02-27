use log::info;

use super::OptimisationProcedure;
use crate::basic_types::CSPSolverExecutionFlag;
use crate::branching::Brancher;
use crate::optimisation::OptimisationDirection;
use crate::predicate;
use crate::results::OptimisationResult;
use crate::results::Solution;
use crate::results::SolutionReference;
use crate::termination::TerminationCondition;
use crate::variables::IntegerVariable;
use crate::Solver;

/// Implements the linear UNSAT-SAT (LUS) optimisation procedure.
#[derive(Debug, Clone, Copy)]
pub struct LinearUnsatSat<Var, Callback> {
    direction: OptimisationDirection,
    objective: Var,
    solution_callback: Callback,
}

impl<Var, Callback> LinearUnsatSat<Var, Callback> {
    /// Create a new instance of [`LinearUnsatSat`].
    pub fn new(
        direction: OptimisationDirection,
        objective: Var,
        solution_callback: Callback,
    ) -> Self {
        Self {
            direction,
            objective,
            solution_callback,
        }
    }
}

impl<Var: IntegerVariable, B: Brancher, Callback: Fn(&Solver, SolutionReference, &B)>
    OptimisationProcedure<Var, B, Callback> for LinearUnsatSat<Var, Callback>
{
    fn optimise(
        &mut self,
        brancher: &mut B,
        termination: &mut impl TerminationCondition,
        solver: &mut Solver,
    ) -> OptimisationResult {
        let is_maximising = matches!(self.direction, OptimisationDirection::Maximise);
        let objective = match self.direction {
            OptimisationDirection::Maximise => self.objective.scaled(-1),
            OptimisationDirection::Minimise => self.objective.scaled(1),
        };
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
            &objective,
            &mut best_objective_value,
            &mut best_solution,
            brancher,
            solver,
        );
        solver.satisfaction_solver.restore_state_at_root(brancher);

        loop {
            let assumption = predicate!(objective <= solver.lower_bound(&objective));

            info!(
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
                        &objective,
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
                        predicate![objective >= best_objective_value as i32 * objective_multiplier]
                    } else {
                        predicate![objective <= best_objective_value as i32 * objective_multiplier]
                    };
                    let _ = solver
                        .satisfaction_solver
                        .conclude_proof_optimal(objective_bound_predicate);

                    return OptimisationResult::Optimal(best_solution);
                }
                CSPSolverExecutionFlag::Infeasible => {
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

    fn on_solution_callback(&self, solver: &Solver, solution: SolutionReference, brancher: &B) {
        (self.solution_callback)(solver, solution, brancher)
    }
}
