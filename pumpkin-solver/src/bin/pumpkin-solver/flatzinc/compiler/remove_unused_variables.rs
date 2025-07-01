//! FlatZinc files can contain variables that are never used in any constraints. This step removes
//! those variables from the AST.
//!
//! To implement this, first we go through all constraints to mark any identifiers they have as
//! arguments. Then, we go through all _marked_ arrays to mark any identifiers contained in the
//! arrays. Finally, we go through the variables and constants and remove them if they are
//! unmarked.
use std::collections::BTreeSet;
use std::rc::Rc;

use super::context::CompilationContext;
use crate::flatzinc::ast::FlatZincAst;
use crate::flatzinc::ast::VarArrayDecl;
use crate::flatzinc::error::FlatZincError;

pub(crate) fn run(
    ast: &mut FlatZincAst,
    context: &mut CompilationContext,
) -> Result<(), FlatZincError> {
    let mut marked_identifiers = BTreeSet::new();

    mark_identifiers_in_constraints(ast, context, &mut marked_identifiers);
    mark_identifiers_in_arrays(ast, context, &mut marked_identifiers);

    ast.single_variables.retain(|decl| match decl {
        crate::flatzinc::ast::SingleVarDecl::Bool { id, .. }
        | crate::flatzinc::ast::SingleVarDecl::IntInRange { id, .. }
        | crate::flatzinc::ast::SingleVarDecl::IntInSet { id, .. } => {
            marked_identifiers.contains(id.as_str())
        }
    });

    Ok(())
}

macro_rules! mark_literal_exprs {
    ($exprs:ident, $expr_type:ident, $identifiers:ident, $context:ident) => {{
        for expr in $exprs {
            if let flatzinc::$expr_type::VarParIdentifier(id) = expr {
                let _ = $identifiers.insert($context.identifiers.get_interned(id));
            }
        }
    }};
}

/// Go over all arrays and mark the identifiers that are elements of the array.
fn mark_identifiers_in_arrays(
    ast: &mut FlatZincAst,
    context: &mut CompilationContext,
    marked_identifiers: &mut BTreeSet<Rc<str>>,
) {
    for array in &ast.variable_arrays {
        match array {
            VarArrayDecl::Bool {
                array_expr: Some(expr),
                ..
            } => match expr {
                flatzinc::ArrayOfBoolExpr::Array(exprs) => {
                    mark_literal_exprs!(exprs, BoolExpr, marked_identifiers, context)
                }
                flatzinc::ArrayOfBoolExpr::VarParIdentifier(_) => {
                    // This is the following case:
                    //
                    // array [1..4] of var int: as = [...];
                    // array [1..4] of var int: bs = as;
                    //
                    // I don't think this can happen, so for now we panic. If it does happen we
                    // need to implement it otherwise we may be removing variables that we need
                    // later on.
                    panic!("Cannot handle array declarations that are assigned to other arrays.")
                }
            },
            VarArrayDecl::Int {
                array_expr: Some(expr),
                ..
            } => match expr {
                flatzinc::ArrayOfIntExpr::Array(exprs) => {
                    mark_literal_exprs!(exprs, IntExpr, marked_identifiers, context)
                }
                flatzinc::ArrayOfIntExpr::VarParIdentifier(_) => {
                    // This is the following case:
                    //
                    // array [1..4] of var int: as = [...];
                    // array [1..4] of var int: bs = as;
                    //
                    // I don't think this can happen, so for now we panic. If it does happen we
                    // need to implement it otherwise we may be removing variables that we need
                    // later on.
                    panic!("Cannot handle array declarations that are assigned to other arrays.")
                }
            },
            _ => {}
        }
    }
}

/// Go over all constraints and add any identifier in the arguments to the `marked_identifiers` set.
fn mark_identifiers_in_constraints(
    ast: &mut FlatZincAst,
    context: &mut CompilationContext,
    marked_identifiers: &mut BTreeSet<Rc<str>>,
) {
    for expr in ast
        .constraint_decls
        .iter()
        .flat_map(|constraint| &constraint.exprs)
    {
        match expr {
            flatzinc::Expr::VarParIdentifier(id) => {
                let _ = marked_identifiers.insert(context.identifiers.get_interned(id));
            }

            flatzinc::Expr::ArrayOfBool(exprs) => {
                mark_literal_exprs!(exprs, BoolExpr, marked_identifiers, context)
            }

            flatzinc::Expr::ArrayOfInt(exprs) => {
                mark_literal_exprs!(exprs, IntExpr, marked_identifiers, context)
            }
            flatzinc::Expr::ArrayOfFloat(exprs) => {
                mark_literal_exprs!(exprs, FloatExpr, marked_identifiers, context)
            }

            flatzinc::Expr::ArrayOfSet(exprs) => {
                mark_literal_exprs!(exprs, SetExpr, marked_identifiers, context)
            }

            flatzinc::Expr::Bool(_)
            | flatzinc::Expr::Int(_)
            | flatzinc::Expr::Float(_)
            | flatzinc::Expr::Set(_) => {}
        }
    }
}
