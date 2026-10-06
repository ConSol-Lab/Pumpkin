//! This compiler pass reserves constraint tags for all the constraints in the model. This is
//! necessary because adding a constraint can cause inferences to be introduced in the proof.
//! However, we assume that the first n constraint tags are the flatzinc constraints. Therefore,
//! the root-level inferences would throw off that mapping.

use super::context::CompilationContext;
use crate::flatzinc::ast::FlatZincAst;
use crate::flatzinc::error::FlatZincError;

pub(crate) fn run(
    ast: &FlatZincAst,
    context: &mut CompilationContext,
) -> Result<(), FlatZincError> {
    for decl in &ast.constraint_decls {
        let tag = context.solver.new_constraint_tag();
        context.constraints.push((tag, decl.clone()));
    }

    Ok(())
}
