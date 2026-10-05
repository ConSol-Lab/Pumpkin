mod constructor;
mod explainer;
mod propagator;
mod rule;
#[cfg(test)]
mod tests;

pub use constructor::IntegerMultiplicationConstructor;
pub use propagator::IntegerMultiplicationPropagator;
pub use rule::*;
