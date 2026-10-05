mod constructor;
mod propagator;
mod rule;
#[cfg(test)]
mod tests;

pub use constructor::*;
pub use propagator::*;
use pumpkin_core::propagation::LocalId;
pub use rule::*;

const ID_NUMERATOR: LocalId = LocalId::from(0);
const ID_DENOMINATOR: LocalId = LocalId::from(1);
const ID_RHS: LocalId = LocalId::from(2);
