//! Contains propagator implementations that are used in Pumpkin.
//!
//! See the [`propagation`] for info on propagators.
#[cfg(doc)]
use pumpkin_core::propagation;

pub mod all_different;
pub mod arithmetic;
pub mod cumulative;
pub mod disjunctive;
pub mod element;
