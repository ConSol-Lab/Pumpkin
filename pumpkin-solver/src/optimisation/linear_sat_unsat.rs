use super::OptimisationProcedure;
use crate::basic_types::CSPSolverExecutionFlag;
use crate::branching::Brancher;
use crate::optimisation::OptimisationDirection;
use crate::predicate;
use crate::pumpkin_assert_simple;
use crate::results::OptimisationResult;
use crate::results::Solution;
use crate::results::SolutionReference;
use crate::termination::TerminationCondition;
use crate::variables::IntegerVariable;
use crate::ConstraintOperationError;
use crate::Solver;

#[derive(Debug, Clone, Copy)]
pub struct LinearSatUnsat<Var: IntegerVariable, Callback> {
    direction: OptimisationDirection,
    objective: Var,
    solution_callback: Callback,
}

impl<Var: IntegerVariable, Callback> LinearSatUnsat<Var, Callback> {
    /// Given the current objective value `best_objective_value`, it adds a constraint specifying
    /// that the objective value should be at most `best_objective_value - 1`. Note that it is
    /// assumed that we are always minimising the variable.
    fn strengthen(
        &mut self,
        objective_variable: &impl IntegerVariable,
        best_objective_value: i64,
        solver: &mut Solver,
    ) -> Result<(), ConstraintOperationError> {
        solver.satisfaction_solver.add_clause([predicate!(
            objective_variable <= (best_objective_value - 1) as i32
        )])
    }

    fn debug_bound_change(
        &self,
        objective_variable: &impl IntegerVariable,
        best_objective_value: i64,
        solver: &Solver,
    ) {
        pumpkin_assert_simple!(
            (solver
                .satisfaction_solver
                .get_assigned_integer_value(objective_variable)
                .expect("expected variable to be assigned") as i64)
                < best_objective_value,
            "{}",
            format!(
                "The current bound {} should be smaller than the previous bound {}",
                solver
                    .satisfaction_solver
                    .get_assigned_integer_value(objective_variable)
                    .expect("expected variable to be assigned"),
                best_objective_value
            )
        );
    }
}

impl<Var, Callback> OptimisationProcedure<Var, Callback> for LinearSatUnsat<Var, Callback>
where
    Var: IntegerVariable,
    Callback: Fn(&Solver, SolutionReference),
{
    fn new(direction: OptimisationDirection, objective: Var, solution_callback: Callback) -> Self {
        Self {
            direction,
            objective,
            solution_callback,
        }
    }

    fn optimise(
        &mut self,
        brancher: &mut impl Brancher,
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

        let initial_solve = solver.satisfaction_solver.solve(termination, brancher);
        match initial_solve {
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

        loop {
            solver.satisfaction_solver.restore_state_at_root(brancher);

            let objective_bound_predicate = if is_maximising {
                predicate![objective >= best_objective_value as i32 * objective_multiplier]
            } else {
                predicate![objective <= best_objective_value as i32 * objective_multiplier]
            };

            if self
                .strengthen(
                    &objective,
                    best_objective_value * objective_multiplier as i64,
                    solver,
                )
                .is_err()
            {
                // Reset the state whenever we return a result
                solver.satisfaction_solver.restore_state_at_root(brancher);
                let _ = solver
                    .satisfaction_solver
                    .conclude_proof_optimal(objective_bound_predicate);
                return OptimisationResult::Optimal(best_solution);
            }

            let solve_result = solver.satisfaction_solver.solve(termination, brancher);
            match solve_result {
                CSPSolverExecutionFlag::Feasible => {
                    self.debug_bound_change(
                        &objective,
                        best_objective_value * objective_multiplier as i64,
                        solver,
                    );
                    self.update_best_solution_and_process(
                        objective_multiplier,
                        &objective,
                        &mut best_objective_value,
                        &mut best_solution,
                        brancher,
                        solver,
                    );
                }
                CSPSolverExecutionFlag::Infeasible => {
                    {
                        // Reset the state whenever we return a result
                        solver.satisfaction_solver.restore_state_at_root(brancher);
                        let _ = solver
                            .satisfaction_solver
                            .conclude_proof_optimal(objective_bound_predicate);
                        return OptimisationResult::Optimal(best_solution);
                    }
                }
                CSPSolverExecutionFlag::Timeout => {
                    // Reset the state whenever we return a result
                    solver.satisfaction_solver.restore_state_at_root(brancher);
                    return OptimisationResult::Satisfiable(best_solution);
                }
            }
        }
    }

    fn on_solution_callback(&self, solver: &Solver, solution: SolutionReference) {
        (self.solution_callback)(solver, solution)
    }
}
