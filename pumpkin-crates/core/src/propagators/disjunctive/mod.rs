use crate::declare_inference_label;

pub(crate) mod disjunctive_propagator;
pub(crate) mod disjunctive_task;
mod theta_lambda_tree;
mod theta_tree;

declare_inference_label!(DisjunctiveEdgeFinding);
