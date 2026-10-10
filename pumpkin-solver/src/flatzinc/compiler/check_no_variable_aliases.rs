//! Under croc proof logging no variable is merged into another: the proof cannot justify the
//! merge. A declaration that makes a variable equal to another, `var 1..5: b = a;`, states an
//! equality no constraint carries, so a model with one is rejected.

use crate::flatzinc::FlatZincError;
use crate::flatzinc::ast::FlatZincAst;
use crate::flatzinc::ast::SingleVarDecl;
use crate::flatzinc::compiler::context::CompilationContext;

pub(crate) fn run(
    ast: &FlatZincAst,
    context: &mut CompilationContext,
) -> Result<(), FlatZincError> {
    for single_var_decl in &ast.single_variables {
        let (id, other) = match single_var_decl {
            SingleVarDecl::Bool {
                id,
                expr: Some(flatzinc::BoolExpr::VarParIdentifier(other)),
                ..
            } => (id, other),
            SingleVarDecl::IntInRange {
                id,
                expr: Some(flatzinc::IntExpr::VarParIdentifier(other)),
                ..
            } => (id, other),
            _ => continue,
        };
        let other = context.identifiers.get_interned(other);
        let is_parameter = context.boolean_parameters.contains_key(&other)
            || context.integer_parameters.contains_key(&other);
        if !is_parameter {
            return Err(FlatZincError::VariableAlias(id.as_str().into()));
        }
    }
    Ok(())
}
