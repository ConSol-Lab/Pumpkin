use super::solution_callback::SolutionCallback;
use super::OptimisationProcedure;
use crate::branching::Brancher;
use crate::optimisation::OptimisationDirection;
use crate::predicate;
use crate::results::OptimisationResult;
use crate::results::ProblemSolution;
use crate::results::SatisfactionResult;
use crate::results::SatisfactionResultUnderAssumptions;
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

impl<Var, B, Callback> OptimisationProcedure<B, Callback> for LinearUnsatSat<Var, Callback>
where
    Var: IntegerVariable,
    B: Brancher,
    Callback: SolutionCallback<B>,
{
    fn optimise(
        &mut self,
        brancher: &mut B,
        termination: &mut impl TerminationCondition,
        solver: &mut Solver,
    ) -> OptimisationResult {
        let objective = match self.direction {
            OptimisationDirection::Maximise => self.objective.scaled(-1),
            OptimisationDirection::Minimise => self.objective.scaled(1),
        };

        let initial_objective_lower_bound = solver.lower_bound(&objective);

        // First we will solve the satisfaction problem without constraining the objective.
        let primal_solution: Solution = match solver.satisfy(brancher, termination) {
            SatisfactionResult::Satisfiable(satisfiable) => satisfiable.solution().into(),
            SatisfactionResult::Unsatisfiable(_) => return OptimisationResult::Unsatisfiable,
            SatisfactionResult::Unknown(_) => return OptimisationResult::Unknown,
        };

        let primal_objective = primal_solution.get_integer_value(objective.clone());

        // Then, we iterate from the lower bound of the objective until (excluding) the primal
        // objective, to find a better solution. The first solution we encounter must be the
        // optimal solution.
        for objective_lower_bound in initial_objective_lower_bound..primal_objective {
            let conclusion = {
                let solve_result = solver.satisfy_under_assumptions(
                    brancher,
                    termination,
                    &[predicate![objective <= objective_lower_bound]],
                );

                match solve_result {
                    SatisfactionResultUnderAssumptions::Satisfiable(satisfiable) => {
                        Some(OptimisationResult::Optimal(satisfiable.solution().into()))
                    }
                    SatisfactionResultUnderAssumptions::UnsatisfiableUnderAssumptions(
                        _,
                    ) => {
                        None
                    }
                    SatisfactionResultUnderAssumptions::Unsatisfiable(_) =>
                        unreachable!("If the problem is unsatisfiable here, it would have been unsatisifable in the initial solve."),
                    SatisfactionResultUnderAssumptions::Unknown(_) => Some(OptimisationResult::Unknown),
                }
            };

            match conclusion {
                Some(OptimisationResult::Optimal(solution)) => {
                    self.solution_callback.on_solution_callback(
                        solver,
                        primal_solution.as_reference(),
                        brancher,
                    );

                    solver.conclude_proof_optimal(predicate![objective >= objective_lower_bound]);
                    return OptimisationResult::Optimal(solution);
                }
                Some(result) => return result,
                None => {}
            }
        }

        solver.conclude_proof_optimal(predicate![objective >= primal_objective]);
        OptimisationResult::Optimal(primal_solution)
    }

    fn on_solution_callback(&self, _: &Solver, _: SolutionReference, _: &B) {
        unreachable!()
    }
}
