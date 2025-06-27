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

/// Implements the linear SAT-UNSAT (LSU) optimisation procedure.
#[derive(Debug, Clone, Copy)]
pub struct LinearSatUnsat<Var, Callback> {
    direction: OptimisationDirection,
    objective: Var,
    solution_callback: Callback,
}

impl<Var, Callback> LinearSatUnsat<Var, Callback> {
    /// Create a new instance of [`LinearSatUnsat`].
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

impl<Var, Callback, B> OptimisationProcedure<B, Callback> for LinearSatUnsat<Var, Callback>
where
    Var: IntegerVariable,
    Var::AffineView: std::fmt::Debug,
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

        // First we will solve the satisfaction problem without constraining the objective.
        let mut best_solution: Solution = match solver.satisfy(brancher, termination) {
            SatisfactionResult::Satisfiable(satisfiable) => satisfiable.solution().into(),
            SatisfactionResult::Unsatisfiable(_) => return OptimisationResult::Unsatisfiable,
            SatisfactionResult::Unknown(_) => return OptimisationResult::Unknown,
        };

        loop {
            self.solution_callback.on_solution_callback(
                solver,
                best_solution.as_reference(),
                brancher,
            );

            let best_objective_value = best_solution.get_integer_value(objective.clone());

            let conclusion = {
                let solve_result = solver.satisfy_under_assumptions(
                    brancher,
                    termination,
                    &[predicate![objective <= best_objective_value - 1]],
                );

                match solve_result {
                    SatisfactionResultUnderAssumptions::Satisfiable(satisfiable) => {
                        best_solution = satisfiable.solution().into();
                        None
                    }
                    SatisfactionResultUnderAssumptions::UnsatisfiableUnderAssumptions(
                        _,
                    ) => {
                        Some(OptimisationResult::Optimal(best_solution.clone()))
                    }
                    SatisfactionResultUnderAssumptions::Unsatisfiable(_) => unreachable!("If the problem is unsatisfiable here, it would have been unsatisifable in the initial solve."),
                    SatisfactionResultUnderAssumptions::Unknown(_) => Some(OptimisationResult::Unknown),
                }
            };

            match conclusion {
                Some(OptimisationResult::Optimal(solution)) => {
                    solver.conclude_proof_optimal(predicate![objective >= best_objective_value]);
                    return OptimisationResult::Optimal(solution);
                }
                Some(result) => return result,
                None => {}
            }
        }
    }

    fn on_solution_callback(&self, _: &Solver, _: SolutionReference, _: &B) {
        unreachable!()
    }
}
