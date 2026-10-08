//! Contains the propagator for the [Element](https://sofdem.github.io/gccat/gccat/Celement.html)
//! constraint.
#![allow(clippy::double_parens, reason = "originates inside the bitfield macro")]
mod checker;
mod constructor;
mod propagator;
#[cfg(test)]
mod tests;

pub use checker::*;
pub use constructor::*;
pub use propagator::*;
