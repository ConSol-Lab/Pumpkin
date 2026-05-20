mod checker;
mod constructor;
mod propagator;

pub use checker::*;
pub use constructor::*;
pub use propagator::*;
use pumpkin_core::declare_inference_label;
use pumpkin_core::propagation::LocalId;

const ID_LHS: LocalId = LocalId::from(0);
const ID_RHS: LocalId = LocalId::from(1);

declare_inference_label!(BinaryEquals);
