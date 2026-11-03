//! Contains the optimisation procedures of the Pumpkin constraint programming solver.
//!
//! It provides implementations of the [`OptimisationProcedure`] trait (defined in
//! `pumpkin-core`), which can be passed to [`Solver::optimise`]:
//! - [`LinearSatUnsat`]: upper-bounding search.
//! - [`LinearUnsatSat`]: lower-bounding search.

#[cfg(doc)]
use pumpkin_core::Solver;
#[cfg(doc)]
use pumpkin_core::optimisation::OptimisationProcedure;

#[cfg(doc)]
use crate::linear_sat_unsat::LinearSatUnsat;
#[cfg(doc)]
use crate::linear_unsat_sat::LinearUnsatSat;

pub mod linear_sat_unsat;
pub mod linear_unsat_sat;

/// The type of search which is performed by the solver.
#[derive(Debug, Clone, Copy, Default)]
#[cfg_attr(feature = "clap", derive(clap::ValueEnum))]
pub enum OptimisationStrategy {
    /// Linear SAT-UNSAT - Starts with a satisfiable solution and tightens the bound on the
    /// objective variable until an UNSAT result is reached.
    ///
    /// Can be seen as upper-bounding search.
    #[default]
    LinearSatUnsat,
    /// Linear UNSAT-SAT - Starts with an unsatisfiable solution and tightens the bound on the
    /// objective variable until a SAT result is reached.
    ///
    /// Can be seen as lower-bounding search.
    LinearUnsatSat,
}

/// The direction of the optimisation, either maximising or minimising.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum OptimisationDirection {
    Maximise,
    Minimise,
}
