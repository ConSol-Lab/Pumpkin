#[cfg(doc)]
use crate::results::unsatisfiable::UnsatisfiableUnderAssumptions;
pub(crate) mod basic_types;
pub mod containers;
pub(crate) mod engine;
pub(crate) mod math;
pub(crate) mod propagators;
pub(crate) mod pumpkin_asserts;

#[cfg(doc)]
use crate::branching::Brancher;
#[cfg(doc)]
use crate::termination::TerminationCondition;

pub mod branching;
pub mod constraints;
pub mod optimisation;
pub mod proof;
pub mod statistics;

pub use convert_case;
pub use rand;

// We declare a private module with public use, so that all exports from API are exports directly
// from the crate.
//
// Example:
// `use pumpkin_solver::Solver;`
// vs.
// `use pumpkin_solver::api::Solver;`
mod api;

pub use api::*;

pub use crate::api::solver::DefaultBrancher;
pub use crate::api::solver::PropagatorHandle;
pub use crate::api::solver::Solver;
pub use crate::basic_types::ConstraintOperationError;
pub use crate::basic_types::Random;
