use pumpkin_core::declare_inference_label;

pub(crate) mod disjunctive_propagator;
pub(crate) mod disjunctive_task;
mod theta_lambda_tree;
mod theta_tree;
pub use disjunctive_propagator::DisjunctiveConstructor;
pub use disjunctive_propagator::DisjunctivePropagator;

declare_inference_label!(DisjunctiveEdgeFinding);
