#[cfg(doc)]
use crate::results::unsatisfiable::UnsatisfiableUnderAssumptions;
pub(crate) mod basic_types;
pub mod containers;
pub(crate) mod engine;
pub(crate) mod math;
pub(crate) mod pumpkin_asserts;

#[cfg(doc)]
use crate::branching::Brancher;
#[cfg(doc)]
use crate::termination::TerminationCondition;

pub mod branching;
pub mod conflict_resolving;
pub mod constraints;
pub mod optimisation;
pub mod proof;
pub mod propagation;
pub mod propagators;
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
pub use crate::api::solver::Solver;
pub use crate::basic_types::ConstraintOperationError;
pub use crate::basic_types::Duration;
pub use crate::basic_types::Instant;
pub use crate::basic_types::Random;
#[allow(deprecated, reason = "Will be refactored in the future")]
pub use crate::engine::SolverStatistics;
#[allow(deprecated, reason = "Will be refactored in the future")]
pub use crate::engine::test_solver::TestSolver;
