//! Merge equivalence classes of each variable definition that refers to another variable.

use flatzinc::ConstraintItem;
use log::warn;

use crate::flatzinc::ast::FlatZincAst;
use crate::flatzinc::ast::SingleVarDecl;
use crate::flatzinc::compiler::context::CompilationContext;
use crate::flatzinc::FlatZincError;
use crate::FlatZincOptions;
use crate::ProofType;

pub(crate) fn run(
    ast: &mut FlatZincAst,
    context: &mut CompilationContext,
    options: &FlatZincOptions,
) -> Result<(), FlatZincError> {
    handle_variable_equality_expressions(ast, context, options)?;
    remove_int_eq_constraints(ast, context, options)?;

    Ok(())
}

/// Used when parsing FlatZinc of the following structure:
///
/// ```
/// var 1..5: a;
/// var 1..5: b = a;
/// ```
///
/// The assignment of `a` to `b` is something we cannot correctly proof-log, as there is no
/// associated constraint tag. Since logging a full proof is not the default, it is likely only
/// done when somebody wants to use the full proof. That is why we panic here instead of log a
/// warning, since the proof would most likely be incorrect.
fn panic_if_logging_proof(options: &FlatZincOptions) {
    if matches!(options.proof_type, Some(ProofType::Full)) {
        panic!("Variable assignment expressions unsupported when logging a full proof.")
    }
}

fn handle_variable_equality_expressions(
    ast: &FlatZincAst,
    context: &mut CompilationContext,
    options: &FlatZincOptions,
) -> Result<(), FlatZincError> {
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

                panic_if_logging_proof(options);

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

                panic_if_logging_proof(options);

                let other_id = context.identifiers.get_interned(identifier);
                context.integer_equivalences.merge(id, other_id);
            }

            SingleVarDecl::IntInSet { .. } => {
                // We do not handle exquivalences for sparse-set domains.
            }
        }
    }

    Ok(())
}

fn remove_int_eq_constraints(
    ast: &mut FlatZincAst,
    context: &mut CompilationContext,
    options: &FlatZincOptions,
) -> Result<(), FlatZincError> {
    if matches!(options.proof_type, Some(ProofType::Full)) {
        return Ok(());
    }

    ast.constraint_decls
        .retain(|constraint| should_keep_constraint(constraint, context));

    Ok(())
}

/// Possibly merges some equivalence classes based on the constraint. Returns `true` if the
/// constraint needs to be retained, and `false` if it can be removed from the AST.
fn should_keep_constraint(constraint: &ConstraintItem, context: &mut CompilationContext) -> bool {
    if constraint.id != "int_eq" {
        return true;
    }

    let v1 = match &constraint.exprs[0] {
        flatzinc::Expr::VarParIdentifier(id) => context.identifiers.get_interned(id),
        flatzinc::Expr::Int(_) => {
            // I don't expect this to be called, but I am not sure. To make it obvious when it does
            // happen, the warning is logged.
            warn!("'int_eq' with constant argument, ignoring it for merging equivalences");
            return true;
        }
        flatzinc::Expr::Float(_)
        | flatzinc::Expr::Bool(_)
        | flatzinc::Expr::Set(_)
        | flatzinc::Expr::ArrayOfBool(_)
        | flatzinc::Expr::ArrayOfInt(_)
        | flatzinc::Expr::ArrayOfFloat(_)
        | flatzinc::Expr::ArrayOfSet(_) => unreachable!(),
    };

    let v2 = match &constraint.exprs[1] {
        flatzinc::Expr::VarParIdentifier(id) => context.identifiers.get_interned(id),
        flatzinc::Expr::Int(_) => {
            // I don't expect this to be called, but I am not sure. To make it obvious when it does
            // happen, the warning is logged.
            warn!("'int_eq' with constant argument, ignoring it for merging equivalences");
            return true;
        }
        flatzinc::Expr::Float(_)
        | flatzinc::Expr::Bool(_)
        | flatzinc::Expr::Set(_)
        | flatzinc::Expr::ArrayOfBool(_)
        | flatzinc::Expr::ArrayOfInt(_)
        | flatzinc::Expr::ArrayOfFloat(_)
        | flatzinc::Expr::ArrayOfSet(_) => unreachable!(),
    };

    context.integer_equivalences.merge(v1, v2);

    false
}

#[cfg(test)]
mod tests {
    use flatzinc::ConstraintItem;
    use flatzinc::Expr;
    use flatzinc::SolveItem;
    use pumpkin_solver::Solver;

    use super::*;

    #[test]
    fn int_eq_constraints_cause_merging_of_equivalence_classes() {
        let mut ast_builder = FlatZincAst::builder();

        ast_builder.add_variable_decl(SingleVarDecl::IntInRange {
            id: "x".into(),
            lb: 1,
            ub: 5,
            expr: None,
            annos: vec![],
        });
        ast_builder.add_variable_decl(SingleVarDecl::IntInRange {
            id: "y".into(),
            lb: 1,
            ub: 5,
            expr: None,
            annos: vec![],
        });
        ast_builder.add_constraint(ConstraintItem {
            id: "int_eq".into(),
            exprs: vec![
                Expr::VarParIdentifier("x".into()),
                Expr::VarParIdentifier("y".into()),
            ],
            annos: vec![],
        });
        ast_builder.set_solve_item(SolveItem {
            goal: flatzinc::Goal::Satisfy,
            annotations: vec![],
        });

        let mut ast = ast_builder.build().expect("valid ast");
        let mut solver = Solver::default();
        let mut context = CompilationContext::new(&mut solver);
        let options = FlatZincOptions::default();

        super::super::prepare_variables::run(&ast, &mut context).expect("step should not fail");
        run(&mut ast, &mut context, &options).expect("step should not fail");

        assert_eq!(
            context.integer_equivalences.representative("x"),
            context.integer_equivalences.representative("y")
        );

        assert!(ast.constraint_decls.is_empty());
    }

    #[test]
    fn int_eq_does_not_merge_when_full_proof_is_being_logged() {
        let mut ast_builder = FlatZincAst::builder();

        ast_builder.add_variable_decl(SingleVarDecl::IntInRange {
            id: "x".into(),
            lb: 1,
            ub: 5,
            expr: None,
            annos: vec![],
        });
        ast_builder.add_variable_decl(SingleVarDecl::IntInRange {
            id: "y".into(),
            lb: 1,
            ub: 5,
            expr: None,
            annos: vec![],
        });
        ast_builder.add_constraint(ConstraintItem {
            id: "int_eq".into(),
            exprs: vec![
                Expr::VarParIdentifier("x".into()),
                Expr::VarParIdentifier("y".into()),
            ],
            annos: vec![],
        });
        ast_builder.set_solve_item(SolveItem {
            goal: flatzinc::Goal::Satisfy,
            annotations: vec![],
        });

        let mut ast = ast_builder.build().expect("valid ast");
        let mut solver = Solver::default();
        let mut context = CompilationContext::new(&mut solver);
        let options = FlatZincOptions {
            proof_type: Some(ProofType::Full),
            ..Default::default()
        };

        super::super::prepare_variables::run(&ast, &mut context).expect("step should not fail");
        run(&mut ast, &mut context, &options).expect("step should not fail");

        assert_ne!(
            context.integer_equivalences.representative("x"),
            context.integer_equivalences.representative("y")
        );

        assert_eq!(ast.constraint_decls.len(), 1);
    }
}
