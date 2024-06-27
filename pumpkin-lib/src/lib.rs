pub(crate) mod basic_types;
pub mod branching;
pub(crate) mod encoders;
pub(crate) mod engine;
pub(crate) mod math;
pub(crate) mod optimisation;
pub(crate) mod propagators;
pub(crate) mod pumpkin_asserts;
pub(crate) mod variable_names;

// We declare a private module with public use, so that all exports from API are exports directly
// from the crate.
//
// Example:
// `use pumpkin_lib::Solver;`
// vs.
// `use pumpkin_lib::api::Solver;`
mod api;

pub use api::*;
