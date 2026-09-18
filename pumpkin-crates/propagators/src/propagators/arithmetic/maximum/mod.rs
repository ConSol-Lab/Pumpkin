mod checker;
mod constructor;
mod propagator;

pub use checker::*;
pub use constructor::*;
pub use propagator::*;
use pumpkin_core::declare_inference_label;

// The inference label for maximum.
declare_inference_label!(Maximum);
