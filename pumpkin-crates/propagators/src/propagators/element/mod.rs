mod constructor;
mod propagator;
mod rule;
#[cfg(test)]
mod tests;

pub use constructor::*;
pub use propagator::*;
use pumpkin_core::propagation::LocalId;
pub use rule::*;

const ID_INDEX: LocalId = LocalId::from(0);
const ID_RHS: LocalId = LocalId::from(1);
// local ids of array vars are shifted by ID_X_OFFSET
const ID_X_OFFSET: u32 = 2;
