mod checker;
mod constraint_description;
mod constructor;
mod explainer;
mod propagator;
mod shared;

pub use checker::IntegerMultiplicationChecker;
pub use constraint_description::*;
pub use constructor::IntegerMultiplicationConstructor;
pub use propagator::IntegerMultiplicationPropagator;

#[cfg(test)]
mod tests;
