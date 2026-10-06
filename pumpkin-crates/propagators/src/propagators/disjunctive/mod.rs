//! Contains the propagator for the [Disjunctive](https://sofdem.github.io/gccat/gccat/Cdisjunctive.html) constraint.
//!
//! Currently, it contains only an edge-finding propagator.

mod conflict_rule;
mod constraint_description;
mod constructor;
pub(crate) mod disjunctive_task;
mod propagator;
#[cfg(test)]
mod tests;
mod theta_lambda_tree;
mod theta_tree;
pub use conflict_rule::*;
pub use constraint_description::*;
pub use constructor::DisjunctiveConstructor;
pub use disjunctive_task::ArgDisjunctiveTask;
pub use propagator::DisjunctivePropagator;
