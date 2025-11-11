//! Compilation phase that processes the parameter declarations into constants.

use std::rc::Rc;

use super::context::CompilationContext;
use super::context::Set;
use crate::flatzinc::FlatZincError;
use crate::flatzinc::ast::FlatZincAst;

pub(crate) fn run(
    ast: &FlatZincAst,
    context: &mut CompilationContext,
) -> Result<(), FlatZincError> {
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

            flatzinc::ParDeclItem::SetOfInt { id, set_literal } => {
                let set = match set_literal {
                    flatzinc::SetLiteral::IntRange(lower_bound, upper_bound) => Set::Interval {
                        lower_bound: i32::try_from(*lower_bound)?,
                        upper_bound: i32::try_from(*upper_bound)?,
                    },

                    flatzinc::SetLiteral::SetInts(values) => {
                        let values = values
                            .iter()
                            .copied()
                            .map(i32::try_from)
                            .collect::<Result<_, _>>()?;

                        Set::Sparse { values }
                    }

                    flatzinc::SetLiteral::BoundedFloat(_, _)
                    | flatzinc::SetLiteral::SetFloats(_) => panic!("float values are unsupported"),
                };

                let _ = context
                    .set_constants
                    .insert(context.identifiers.get_interned(id), set);
            }

            flatzinc::ParDeclItem::ArrayOfSet { .. } => {
                todo!("implement array of integer set parameters")
            }

            flatzinc::ParDeclItem::Float { .. } | flatzinc::ParDeclItem::ArrayOfFloat { .. } => {
                panic!("floats are not supported")
            }
        }
    }

    Ok(())
}
