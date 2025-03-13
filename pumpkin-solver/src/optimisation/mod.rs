//! Contains structures related to optimissation.
use std::fmt::Display;

use clap::ValueEnum;
use solution_callback::SolutionCallback;

use crate::branching::Brancher;
use crate::results::OptimisationResult;
use crate::results::Solution;
use crate::results::SolutionReference;
use crate::termination::TerminationCondition;
use crate::variables::IntegerVariable;
use crate::Solver;

pub mod linear_sat_unsat;
pub mod linear_unsat_sat;
pub mod solution_callback;

pub trait OptimisationProcedure<B: Brancher, Callback: SolutionCallback<B>> {
    fn optimise(
        &mut self,
        brancher: &mut B,
        termination: &mut impl TerminationCondition,
        solver: &mut Solver,
    ) -> OptimisationResult;

    fn on_solution_callback(&self, solver: &Solver, solution: SolutionReference, brancher: &B);

    /// Processes a solution when it is found, it consists of the following procedure:
    /// - Assigning `best_objective_value` the value assigned to `objective_variable` (multiplied by
    ///   `objective_multiplier`).
    /// - Storing the new best solution in `best_solution`.
    /// - Calling [`Brancher::on_solution`] on the provided `brancher`.
    /// - Logging the statistics using [`Solver::log_statistics_with_objective`].
    /// - Calling the solution callback.
    fn update_best_solution_and_process(
        &self,
        objective_multiplier: i32,
        objective_variable: &impl IntegerVariable,
        best_objective_value: &mut i64,
        best_solution: &mut Solution,
        brancher: &mut B,
        solver: &Solver,
    ) {
        *best_objective_value = (objective_multiplier
            * solver
                .satisfaction_solver
                .get_assigned_integer_value(objective_variable)
                .expect("expected variable to be assigned")) as i64;
        *best_solution = solver.satisfaction_solver.get_solution_reference().into();

        self.internal_process_solution(best_solution, brancher, solver)
    }

    fn internal_process_solution(&self, solution: &Solution, brancher: &mut B, solver: &Solver) {
        brancher.on_solution(solution.as_reference());

        self.on_solution_callback(solver, solution.as_reference(), brancher)
    }
}

/// The type of search which is performed by the solver.
#[derive(Debug, Clone, Copy, ValueEnum, Default)]
pub enum OptimisationStrategy {
    /// Linear SAT-UNSAT - Starts with a satisfiable solution and tightens the bound on the
    /// objective variable until an UNSAT result is reached. Can be seen as upper-bounding search.
    #[default]
    LinearSatUnsat,
    /// Linear UNSAT-SAT - Starts with an unsatisfiable solution and tightens the bound on the
    /// objective variable until a SAT result is reached. Can be seen as lower-bounding search.
    LinearUnsatSat,
}

impl Display for OptimisationStrategy {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OptimisationStrategy::LinearSatUnsat => write!(f, "linear-sat-unsat"),
            OptimisationStrategy::LinearUnsatSat => write!(f, "linear-unsat-sat"),
        }
    }
}

/// The direction of the optimisation, either maximising or minimising.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum OptimisationDirection {
    Maximise,
    Minimise,
}
