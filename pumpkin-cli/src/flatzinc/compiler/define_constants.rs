//! Compilation phase that processes the parameter declarations into constants.

use std::rc::Rc;

use super::context::CompilationContext;
use crate::flatzinc::ast::FlatZincAst;
use crate::flatzinc::FlatZincError;

pub fn run(ast: &FlatZincAst, context: &mut CompilationContext) -> Result<(), FlatZincError> {
    for parameter_decl in &ast.parameter_decls {
        match parameter_decl {
            flatzinc::ParDeclItem::Bool { id, bool } => {
                let _ = context
                    .boolean_parameters
                    .insert(context.identifiers.get_interned(id), *bool);
            }

            flatzinc::ParDeclItem::Int { id, int } => {
                let value = i32::try_from(*int)?;

                let _ = context
                    .integer_parameters
                    .insert(context.identifiers.get_interned(id), value);
            }

            flatzinc::ParDeclItem::ArrayOfBool { id, v, .. } => {
                let _ = context
                    .boolean_array_parameters
                    .insert(context.identifiers.get_interned(id), v.clone().into());
            }

            flatzinc::ParDeclItem::ArrayOfInt { id, v, .. } => {
                let value = v
                    .iter()
                    .map(|value| i32::try_from(*value))
                    .collect::<Result<Rc<[_]>, _>>()?;

                let _ = context
                    .integer_array_parameters
                    .insert(context.identifiers.get_interned(id), value);
            }

            flatzinc::ParDeclItem::SetOfInt { .. } | flatzinc::ParDeclItem::ArrayOfSet { .. } => {
                todo!("implement integer set parameters")
            }

            flatzinc::ParDeclItem::Float { .. } | flatzinc::ParDeclItem::ArrayOfFloat { .. } => {
                panic!("floats are not supported")
            }
        }
    }

    Ok(())
}
