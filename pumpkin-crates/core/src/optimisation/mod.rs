//! Contains structures related to optimissation.

use solution_callback::SolutionCallback;

use crate::branching::Brancher;
use crate::results::OptimisationResult;
use crate::termination::TerminationCondition;
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
}

/// The type of search which is performed by the solver.
#[derive(Debug, Clone, Copy, Default)]
#[cfg_attr(feature = "clap", derive(clap::ValueEnum))]
pub enum OptimisationStrategy {
    /// Linear SAT-UNSAT - Starts with a satisfiable solution and tightens the bound on the
    /// objective variable until an UNSAT result is reached. Can be seen as upper-bounding search.
    #[default]
    LinearSatUnsat,
    /// Linear UNSAT-SAT - Starts with an unsatisfiable solution and tightens the bound on the
    /// objective variable until a SAT result is reached. Can be seen as lower-bounding search.
    LinearUnsatSat,
}

/// The direction of the optimisation, either maximising or minimising.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum OptimisationDirection {
    Maximise,
    Minimise,
}
