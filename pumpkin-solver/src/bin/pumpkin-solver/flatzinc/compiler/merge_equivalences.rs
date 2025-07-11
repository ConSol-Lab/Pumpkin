//! Merge equivalence classes of each variable definition that refers to another variable.

use std::rc::Rc;

use fzn_rs::ast;
use fzn_rs::VariableArgument;
use log::warn;

use crate::flatzinc::ast::Constraints;
use crate::flatzinc::ast::Instance;
use crate::flatzinc::compiler::context::CompilationContext;
use crate::flatzinc::FlatZincError;
use crate::FlatZincOptions;
use crate::ProofType;

pub(crate) fn run(
    typed_ast: &mut Instance,
    context: &mut CompilationContext,
    options: &FlatZincOptions,
) -> Result<(), FlatZincError> {
    handle_variable_equality_expressions(typed_ast, context, options)?;
    remove_int_eq_constraints(typed_ast, context, options)?;

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
    typed_ast: &Instance,
    context: &mut CompilationContext,
    options: &FlatZincOptions,
) -> Result<(), FlatZincError> {
    for (name, variable) in typed_ast.variables.iter() {
        let other_variable = match &variable.value {
            Some(ast::Node {
                node: ast::Literal::Identifier(id),
                ..
            }) => Rc::clone(id),
            _ => continue,
        };

        match variable.domain.node {
            ast::Domain::Bool => {
                if !context.literal_equivalences.is_defined(&other_variable) {
                    return Err(FlatZincError::InvalidIdentifier {
                        identifier: other_variable.as_ref().into(),
                        expected_type: "var bool".into(),
                    });
                }

                panic_if_logging_proof(options);

                context
                    .literal_equivalences
                    .merge(other_variable, Rc::clone(name));
            }

            ast::Domain::Int(_) => {
                if !context.integer_equivalences.is_defined(&other_variable) {
                    return Err(FlatZincError::InvalidIdentifier {
                        identifier: other_variable.as_ref().into(),
                        expected_type: "var bool".into(),
                    });
                }

                panic_if_logging_proof(options);

                context
                    .integer_equivalences
                    .merge(other_variable, Rc::clone(name));
            }

            ast::Domain::UnboundedInt => {
                return Err(FlatZincError::UnsupportedVariable(name.as_ref().into()))
            }
        }
    }

    Ok(())
}

fn remove_int_eq_constraints(
    typed_ast: &mut Instance,
    context: &mut CompilationContext,
    options: &FlatZincOptions,
) -> Result<(), FlatZincError> {
    if matches!(options.proof_type, Some(ProofType::Full)) {
        return Ok(());
    }

    typed_ast
        .constraints
        .retain(|constraint| should_keep_constraint(constraint, context));

    Ok(())
}

/// Possibly merges some equivalence classes based on the constraint. Returns `true` if the
/// constraint needs to be retained, and `false` if it can be removed from the AST.
fn should_keep_constraint<Ann>(
    constraint: &fzn_rs::Constraint<Constraints, Ann>,
    context: &mut CompilationContext,
) -> bool {
    let Constraints::IntEq(lhs, rhs) = &constraint.constraint.node else {
        return true;
    };

    let v1 = match lhs {
        VariableArgument::Identifier(id) => Rc::clone(id),
        VariableArgument::Constant(_) => {
            // I don't expect this to be called, but I am not sure. To make it obvious when it does
            // happen, the warning is logged.
            warn!("'int_eq' with constant argument, ignoring it for merging equivalences");
            return true;
        }
    };

    let v2 = match rhs {
        VariableArgument::Identifier(id) => Rc::clone(id),
        VariableArgument::Constant(_) => {
            // I don't expect this to be called, but I am not sure. To make it obvious when it does
            // happen, the warning is logged.
            warn!("'int_eq' with constant argument, ignoring it for merging equivalences");
            return true;
        }
    };

    context.integer_equivalences.merge(v1, v2);

    false
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;

    use flatzinc::ConstraintItem;
    use flatzinc::Expr;
    use flatzinc::SolveItem;
    use fzn_rs::Constraint;
    use pumpkin_solver::Solver;

    use super::*;

    #[test]
    fn int_eq_constraints_cause_merging_of_equivalence_classes() {
        let mut instance = Instance {
            variables: BTreeMap::from([
                (
                    "x".into(),
                    ast::Variable {
                        domain: test_node(ast::Domain::Int(ast::RangeList::from(1..=5))),
                        value: None,
                        annotations: vec![],
                    },
                ),
                (
                    "y".into(),
                    ast::Variable {
                        domain: test_node(ast::Domain::Int(ast::RangeList::from(1..=5))),
                        value: None,
                        annotations: vec![],
                    },
                ),
            ]),
            constraints: vec![Constraint {
                constraint: test_node(Constraints::IntEq(
                    VariableArgument::Identifier("x".into()),
                    VariableArgument::Identifier("y".into()),
                )),
                annotations: vec![],
            }],
            solve: ast::SolveObjective {
                method: test_node(ast::Method::Satisfy),
                annotations: vec![],
            },
        };

        let mut solver = Solver::default();
        let mut context = CompilationContext::new(&mut solver);
        let options = FlatZincOptions::default();

        super::super::prepare_variables::run(&instance, &mut context)
            .expect("step should not fail");
        run(&mut instance, &mut context, &options).expect("step should not fail");

        assert_eq!(
            context.integer_equivalences.representative("x"),
            context.integer_equivalences.representative("y")
        );

        assert!(instance.constraints.is_empty());
    }

    #[test]
    fn int_eq_does_not_merge_when_full_proof_is_being_logged() {
        let mut instance = Instance {
            variables: BTreeMap::from([
                (
                    "x".into(),
                    ast::Variable {
                        domain: test_node(ast::Domain::Int(ast::RangeList::from(1..=5))),
                        value: None,
                        annotations: vec![],
                    },
                ),
                (
                    "y".into(),
                    ast::Variable {
                        domain: test_node(ast::Domain::Int(ast::RangeList::from(1..=5))),
                        value: None,
                        annotations: vec![],
                    },
                ),
            ]),
            constraints: vec![Constraint {
                constraint: test_node(Constraints::IntEq(
                    VariableArgument::Identifier("x".into()),
                    VariableArgument::Identifier("y".into()),
                )),
                annotations: vec![],
            }],
            solve: ast::SolveObjective {
                method: test_node(ast::Method::Satisfy),
                annotations: vec![],
            },
        };

        let mut solver = Solver::default();
        let mut context = CompilationContext::new(&mut solver);
        let options = FlatZincOptions {
            proof_type: Some(ProofType::Full),
            ..Default::default()
        };

        super::super::prepare_variables::run(&instance, &mut context)
            .expect("step should not fail");
        run(&mut instance, &mut context, &options).expect("step should not fail");

        assert_ne!(
            context.integer_equivalences.representative("x"),
            context.integer_equivalences.representative("y")
        );

        assert_eq!(instance.constraints.len(), 1);
    }

    fn test_node<T>(data: T) -> ast::Node<T> {
        ast::Node {
            node: data,
            span: ast::Span { start: 0, end: 0 },
        }
    }
}
