//! Merge equivalence classes of each variable definition that refers to another variable.

use crate::flatzinc::ast::FlatZincAst;
use crate::flatzinc::ast::SingleVarDecl;
use crate::flatzinc::compiler::context::CompilationContext;
use crate::flatzinc::FlatZincError;

pub fn run(ast: &FlatZincAst, context: &mut CompilationContext) -> Result<(), FlatZincError> {
    for single_var_decl in &ast.single_variables {
        match single_var_decl {
            SingleVarDecl::Bool { id, expr, .. } => {
                let id = context.identifiers.get_interned(id);

                let Some(flatzinc::BoolExpr::VarParIdentifier(identifier)) = expr else {
                    continue;
                };

                if !context.literal_equivalences.is_defined(&id)
                    && context.boolean_parameters.contains_key(&id)
                {
                    // The identifier points to a parameter.
                    continue;
                }

                if !context.literal_equivalences.is_defined(&id) {
                    return Err(FlatZincError::InvalidIdentifier {
                        identifier: id.as_ref().into(),
                        expected_type: "var bool".into(),
                    });
                }

                let other_id = context.identifiers.get_interned(identifier);
                context.literal_equivalences.merge(id, other_id);
            }

            SingleVarDecl::IntInRange { id, expr, .. } => {
                let id = context.identifiers.get_interned(id);

                let Some(flatzinc::IntExpr::VarParIdentifier(identifier)) = expr else {
                    continue;
                };

                if !context.integer_equivalences.is_defined(&id)
                    && context.integer_parameters.contains_key(&id)
                {
                    // The identifier points to a parameter.
                    continue;
                }

                if !context.integer_equivalences.is_defined(&id) {
                    return Err(FlatZincError::InvalidIdentifier {
                        identifier: id.as_ref().into(),
                        expected_type: "var bool".into(),
                    });
                }

                let other_id = context.identifiers.get_interned(identifier);
                context.integer_equivalences.merge(id, other_id);
            }
        }
    }
    Ok(())
}
