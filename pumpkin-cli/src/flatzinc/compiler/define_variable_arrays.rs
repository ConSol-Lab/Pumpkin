//! Compilation phase that processes the array variable declarations into literals/variables.

use std::rc::Rc;

use flatzinc::AnnExpr;
use flatzinc::Annotation;
use flatzinc::ArrayOfBoolExpr;
use flatzinc::ArrayOfIntExpr;
use flatzinc::BoolExpr;
use flatzinc::Expr;
use flatzinc::IntExpr;
use flatzinc::SetExpr;
use flatzinc::SetLiteralExpr;

use super::context::CompilationContext;
use crate::flatzinc::ast::FlatZincAst;
use crate::flatzinc::ast::VarArrayDecl;
use crate::flatzinc::instance::Output;
use crate::flatzinc::FlatZincError;

pub fn run(ast: &FlatZincAst, context: &mut CompilationContext) -> Result<(), FlatZincError> {
    for array_decl in &ast.variable_arrays {
        match array_decl {
            VarArrayDecl::Bool {
                ix: _,
                id,
                annos,
                array_expr,
            } => {
                let id = context.identifiers.get_interned(id);
                let contents: Rc<[_]> =
                    match array_expr.as_ref().expect("array did not have expression") {
                        ArrayOfBoolExpr::Array(array) => array
                            .iter()
                            .map(|expr| match expr {
                                BoolExpr::Bool(true) => context.constant_bool_true,
                                BoolExpr::Bool(false) => context.constant_bool_false,
                                BoolExpr::VarParIdentifier(identifier) => {
                                    let other_id = context.identifiers.get_interned(identifier);
                                    let representative =
                                        context.literal_equivalences.representative(&other_id);

                                    context
                                        .boolean_variable_map
                                        .get(&representative)
                                        .copied()
                                        .expect("referencing undefined boolean variable")
                                }
                            })
                            .collect(),

                        ArrayOfBoolExpr::VarParIdentifier(_) => {
                            todo!("array of boolean variable expression is identifier")
                        }
                    };

                if let Some(shape) = is_output_array(annos) {
                    context.outputs.push(Output::array_of_bool(
                        id.clone(),
                        shape,
                        contents.clone(),
                    ));
                }

                context.boolean_variable_arrays.insert(id, contents);
            }

            VarArrayDecl::Int {
                ix: _,
                id,
                annos,
                array_expr,
            }
            | VarArrayDecl::IntInRange {
                ix: _,
                id,
                annos,
                array_expr,
                // Bounds on the type here are ignored, instead the bounds on the variables in the
                // array are used.
                lb: _,
                ub: _,
            } => {
                let id = context.identifiers.get_interned(id);
                let contents =
                    match array_expr.as_ref().expect("array did not have expression") {
                        ArrayOfIntExpr::Array(array) => array
                            .iter()
                            .map(|expr| match expr {
                                IntExpr::Int(int) => {
                                    let value = i32::try_from(*int)?;

                                    Ok(*context.constant_domain_ids.entry(value).or_insert_with(
                                        || context.solver.create_new_integer_variable(value, value),
                                    ))
                                }
                                IntExpr::VarParIdentifier(identifier) => {
                                    let other_id = context.identifiers.get_interned(identifier);
                                    let representative =
                                        context.integer_equivalences.representative(&other_id);

                                    Ok(context
                                        .integer_variable_map
                                        .get(&representative)
                                        .copied()
                                        .expect("referencing undefined boolean variable"))
                                }
                            })
                            .collect::<Result<Rc<[_]>, FlatZincError>>()?,

                        ArrayOfIntExpr::VarParIdentifier(_) => {
                            todo!("array of integer variable expression is identifier")
                        }
                    };

                if let Some(shape) = is_output_array(annos) {
                    context
                        .outputs
                        .push(Output::array_of_int(id.clone(), shape, contents.clone()));
                }

                context.integer_variable_arrays.insert(id, contents);
            }
        }
    }

    Ok(())
}

fn is_output_array(annos: &[Annotation]) -> Option<Box<[(i32, i32)]>> {
    annos.iter().find_map(|annotation| {
        if annotation.id == "output_array" {
            assert_eq!(1, annotation.expressions.len());

            match &annotation.expressions[0] {
                AnnExpr::Annotations(_) | AnnExpr::String(_) => {
                    panic!("expected a list of integer intervals in output_array annotation")
                }

                AnnExpr::Expr(expr) => match expr {
                    Expr::VarParIdentifier(_)
                    | Expr::Bool(_)
                    | Expr::Int(_)
                    | Expr::Float(_)
                    | Expr::Set(_)
                    | Expr::ArrayOfBool(_)
                    | Expr::ArrayOfInt(_)
                    | Expr::ArrayOfFloat(_) => panic!(
                        "expected an array of sets as the argument to the output_array annotation"
                    ),

                    Expr::ArrayOfSet(sets) => Some(
                        sets.iter()
                            .map(|set| match set {
                                SetExpr::Set(set) => match set {
                                    SetLiteralExpr::BoundedFloat(_, _)
                                    | SetLiteralExpr::SetFloats(_)
                                    | SetLiteralExpr::SetInts(_) => panic!(
                                        "expected interval set as the index sets for the output_array annotation"
                                    ),

                                    SetLiteralExpr::IntInRange(min, max) => match (min, max) {
                                        (IntExpr::Int(min), IntExpr::Int(max)) => (min, max),

                                        _ => panic!("expected interval sets to be delimited with integer expressions, not identifiers"),
                                    },
                                },
                                SetExpr::VarParIdentifier(_) => panic!(
                                    "identifiers are not supported as the argument to output_array"
                                ),
                            })
                            .map(|(&min, &max)| {
                                (i32::try_from(min).expect("integer too large"),
                                 i32::try_from(max).expect("integer too large"))
                            })
                            .collect(),
                    ),
                },
            }
        } else {
            None
        }
    })
}
