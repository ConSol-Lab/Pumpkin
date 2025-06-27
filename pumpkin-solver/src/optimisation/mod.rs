//! Contains structures related to optimissation.
use std::fmt::Display;

use clap::ValueEnum;
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
