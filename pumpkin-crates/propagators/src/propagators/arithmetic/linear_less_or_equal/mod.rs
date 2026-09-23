mod constructor;
mod propagator;
#[cfg(test)]
mod tests;

pub use constructor::*;
pub use propagator::*;
use pumpkin_core::declare_inference_label;

declare_inference_label!(LinearBounds);
