//! Contains the propagator for the [Disjunctive](https://sofdem.github.io/gccat/gccat/Cdisjunctive.html) constraint.
//!
//! Currently, it contains only an edge-finding propagator.
use pumpkin_core::declare_inference_label;

pub(crate) mod disjunctive_propagator;
pub(crate) mod disjunctive_task;
mod theta_lambda_tree;
mod theta_tree;
pub use disjunctive_propagator::DisjunctiveConstructor;
pub use disjunctive_propagator::DisjunctivePropagator;
pub use disjunctive_task::ArgDisjunctiveTask;

declare_inference_label!(DisjunctiveEdgeFinding);
