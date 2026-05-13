use std::ops::ControlFlow;

use super::OptimisationProcedure;
use super::solution_callback::SolutionCallback;
use crate::Solver;
use crate::branching::Brancher;
use crate::conflict_resolving::ConflictResolver;
use crate::optimisation::OptimisationDirection;
use crate::predicate;
use crate::results::OptimisationResult;
use crate::results::ProblemSolution;
use crate::results::SatisfactionResult;
use crate::results::SatisfactionResultUnderAssumptions;
use crate::results::Solution;
use crate::termination::TerminationCondition;
use crate::variables::IntegerVariable;

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

    fn run_optimisation<B, R>(
        &mut self,
        brancher: &mut B,
        termination: &mut impl TerminationCondition,
        resolver: &mut R,
        solver: &mut Solver,
        objective: impl IntegerVariable,
        mut best_solution: Solution,
    ) -> OptimisationResult<Callback::Stop>
    where
        Callback: SolutionCallback<B, R>,
        B: Brancher,
        R: ConflictResolver,
    {
        loop {
            let callback_result = self.solution_callback.on_solution_callback(
                solver,
                best_solution.as_reference(),
                brancher,
                resolver,
            );

            if let ControlFlow::Break(stop) = callback_result {
                return OptimisationResult::Stopped(best_solution, stop);
            }

            let best_objective_value = best_solution.get_integer_value(objective.clone());

            let conclusion = {
                let solve_result = solver.satisfy_under_assumptions(
                    brancher,
                    termination,
                    resolver,
                    &[predicate![objective <= best_objective_value - 1]],
                );

                match solve_result {
                    SatisfactionResultUnderAssumptions::Satisfiable(satisfiable) => {
                        best_solution = satisfiable.solution().into();
                        None
                    }
                    SatisfactionResultUnderAssumptions::UnsatisfiableUnderAssumptions(_) => {
                        Some(OptimisationResult::Optimal(best_solution.clone()))
                    }
                    SatisfactionResultUnderAssumptions::Unsatisfiable(_) => unreachable!(
                        "If the problem is unsatisfiable here, it would have been unsatisifable in the initial solve."
                    ),
                    SatisfactionResultUnderAssumptions::Unknown(_) => {
                        Some(OptimisationResult::Satisfiable(best_solution.clone()))
                    }
                }
            };

            match conclusion {
                Some(OptimisationResult::Optimal(solution)) => {
                    return OptimisationResult::Optimal(solution);
                }
                Some(result) => return result,
                None => {}
            }
        }
    }
}

impl<Var, Callback, B, R> OptimisationProcedure<B, R, Callback> for LinearSatUnsat<Var, Callback>
where
    Var: IntegerVariable,
    B: Brancher,
    R: ConflictResolver,
    Callback: SolutionCallback<B, R>,
    Callback::Stop: std::fmt::Debug,
{
    fn optimise(
        &mut self,
        brancher: &mut B,
        termination: &mut impl TerminationCondition,
        resolver: &mut R,
        solver: &mut Solver,
    ) -> OptimisationResult<Callback::Stop> {
        let objective = match self.direction {
            OptimisationDirection::Maximise => self.objective.scaled(-1),
            OptimisationDirection::Minimise => self.objective.scaled(1),
        };

        // First we will solve the satisfaction problem without constraining the objective.
        let initial_solution: Solution = match solver.satisfy(brancher, termination, resolver) {
            SatisfactionResult::Satisfiable(satisfiable) => satisfiable.solution().into(),
            SatisfactionResult::Unsatisfiable(_, _, _) => return OptimisationResult::Unsatisfiable,
            SatisfactionResult::Unknown(_, _, _) => return OptimisationResult::Unknown,
        };

        let optimisation_result = self.run_optimisation(
            brancher,
            termination,
            resolver,
            solver,
            objective.clone(),
            initial_solution,
        );

        match optimisation_result {
            OptimisationResult::Optimal(solution) => {
                let objective_value = solution.get_integer_value(objective.clone());
                solver.conclude_proof_dual_bound(predicate![objective >= objective_value]);

                OptimisationResult::Optimal(solution)
            }
            OptimisationResult::Unsatisfiable => {
                solver.conclude_proof_unsat();
                OptimisationResult::Unsatisfiable
            }

            result @ (OptimisationResult::Satisfiable(_)
            | OptimisationResult::Stopped(_, _)
            | OptimisationResult::Unknown) => result,
        }
    }
}
