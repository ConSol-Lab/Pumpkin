mod constructor;
mod inference_checker;
mod propagator;

pub use constructor::*;
pub use propagator::*;
use pumpkin_core::declare_inference_label;
use pumpkin_core::propagation::LocalId;

// The LocalId's for the variables.
const ID_A: LocalId = LocalId::from(0);
const ID_B: LocalId = LocalId::from(1);
const ID_C: LocalId = LocalId::from(2);

// The inference label for integer multiplication.
declare_inference_label!(IntegerMultiplication);
