//! Contains the propagator for the [Disjunctive](https://sofdem.github.io/gccat/gccat/Cdisjunctive.html) constraint.
//!
//! Currently, it contains only an edge-finding propagator.
use pumpkin_core::declare_inference_label;

mod constructor;
pub(crate) mod disjunctive_task;
mod propagator;
#[cfg(test)]
mod tests;
mod theta_lambda_tree;
mod theta_tree;
pub use constructor::DisjunctiveConstructor;
pub use disjunctive_task::ArgDisjunctiveTask;
pub use propagator::DisjunctivePropagator;

declare_inference_label!(DisjunctiveEdgeFinding);
