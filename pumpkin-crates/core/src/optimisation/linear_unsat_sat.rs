use std::num::NonZero;
use std::ops::ControlFlow;

use super::OptimisationProcedure;
use super::solution_callback::SolutionCallback;
use crate::Solver;
use crate::branching::Brancher;
use crate::optimisation::OptimisationDirection;
use crate::predicate;
use crate::proof::ConstraintTag;
use crate::results::OptimisationResult;
use crate::results::ProblemSolution;
use crate::results::SatisfactionResult;
use crate::results::SatisfactionResultUnderAssumptions;
use crate::results::Solution;
use crate::termination::TerminationCondition;
use crate::variables::IntegerVariable;

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
    ) -> OptimisationResult<Callback::Stop> {
        let objective = match self.direction {
            OptimisationDirection::Maximise => self.objective.scaled(-1),
            OptimisationDirection::Minimise => self.objective.scaled(1),
        };

        // First we will solve the satisfaction problem without constraining the objective.
        let primal_solution: Solution = match solver.satisfy(brancher, termination) {
            SatisfactionResult::Satisfiable(satisfiable) => satisfiable.solution().into(),
            SatisfactionResult::Unsatisfiable(_, _) => return OptimisationResult::Unsatisfiable,
            SatisfactionResult::Unknown(_, _) => return OptimisationResult::Unknown,
        };

        let callback_result = self.solution_callback.on_solution_callback(
            solver,
            primal_solution.as_reference(),
            brancher,
        );

        if let ControlFlow::Break(stop) = callback_result {
            return OptimisationResult::Stopped(primal_solution, stop);
        }

        let primal_objective = primal_solution.get_integer_value(objective.clone());

        // Then, we iterate from the lower bound of the objective until (excluding) the primal
        // objective, to find a better solution. The first solution we encounter must be the
        // optimal solution.
        //
        // We purposefully start at one less than the initial lower bound of the objective. Even
        // though this is a trivial case, this first iteration will output the correct steps to the
        // proof that allow us to conclude a lower bound even if we are never able to conclude
        // unsat for `objective <= initial_objective_lower_bound`. If we did not have this, then
        // the proof would not contain the initial lower bound as an inference, and the dual bound
        // claim can therefore not be checked.

        let mut objective_lower_bound = solver.lower_bound(&objective);
        let mut proven_lower_bound = objective_lower_bound;

        while objective_lower_bound < primal_objective {
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
                    SatisfactionResultUnderAssumptions::UnsatisfiableUnderAssumptions(_) => None,
                    SatisfactionResultUnderAssumptions::Unsatisfiable(_) => unreachable!(
                        "If the problem is unsatisfiable here, it would have been unsatisifable in the initial solve."
                    ),
                    SatisfactionResultUnderAssumptions::Unknown(_) => {
                        Some(OptimisationResult::Unknown)
                    }
                }
            };

            match conclusion {
                Some(OptimisationResult::Optimal(solution)) => {
                    // Optimisation will stop regardless of the result of the callback.
                    let _ = self.solution_callback.on_solution_callback(
                        solver,
                        primal_solution.as_reference(),
                        brancher,
                    );

                    solver
                        .conclude_proof_dual_bound(predicate![objective >= objective_lower_bound]);
                    return OptimisationResult::Optimal(solution);
                }
                Some(OptimisationResult::Unknown) => {
                    solver.conclude_proof_dual_bound(predicate![objective >= proven_lower_bound]);
                    return OptimisationResult::Satisfiable(primal_solution);
                }
                Some(result) => return result,
                None => {}
            }

            solver
                .add_clause(
                    [predicate![objective >= objective_lower_bound + 1]],
                    ConstraintTag::from_non_zero(NonZero::<u32>::MAX),
                )
                .expect("this should always be valid given the previous solves");

            proven_lower_bound = objective_lower_bound + 1;
            objective_lower_bound = solver.lower_bound(&objective);
        }

        solver.conclude_proof_dual_bound(predicate![objective >= primal_objective]);
        OptimisationResult::Optimal(primal_solution)
    }
}
