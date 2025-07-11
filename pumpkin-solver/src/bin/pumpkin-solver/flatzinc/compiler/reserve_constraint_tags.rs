//! This compiler pass reserves constraint tags for all the constraints in the model. This is
//! necessary because adding a constraint can cause inferences to be introduced in the proof.
//! However, we assume that the first n constraint tags are the flatzinc constraints. Therefore,
//! the root-level inferences would throw off that mapping.

use fzn_rs::ast;

use super::context::CompilationContext;
use crate::flatzinc::{
    ast::{ConstraintAnnotations, Instance},
    error::FlatZincError,
};

pub(crate) fn run(
    instance: &mut Instance,
    context: &mut CompilationContext,
) -> Result<(), FlatZincError> {
    for constraint in instance.constraints.iter_mut() {
        let tag = context.solver.new_constraint_tag();
        constraint
            .annotations
            .push(generated_node(ConstraintAnnotations::ConstraintTag(
                tag.into(),
            )));
    }

    Ok(())
}

fn generated_node<T>(data: T) -> ast::Node<T> {
    ast::Node {
        span: ast::Span {
            start: usize::MAX,
            end: usize::MAX,
        },
        node: data,
    }
}
