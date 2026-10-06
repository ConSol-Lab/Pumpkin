mod conflict_rule;
mod constraint_description;
mod constructor;
mod propagator;
#[cfg(test)]
mod tests;

pub use conflict_rule::*;
pub use constraint_description::*;
pub use constructor::*;
pub use propagator::*;
use pumpkin_core::propagation::LocalId;

const ID_NUMERATOR: LocalId = LocalId::from(0);
const ID_DENOMINATOR: LocalId = LocalId::from(1);
const ID_RHS: LocalId = LocalId::from(2);
