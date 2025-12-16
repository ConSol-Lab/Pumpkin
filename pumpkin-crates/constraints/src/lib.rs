//! Contains certain constraints which can be used by the Pumpkin solver.
//!
//! A constraint enforces a relationship between variables. It is defined by one or more
//! propagators (see [`pumpkin_propagators`]).
mod constraints;
pub use constraints::*;
