//! Scan through all the variable definitions to prepare the equivalence classes for each of them.

use std::rc::Rc;

use fzn_rs::ast;

use crate::flatzinc::ast::Instance;
use crate::flatzinc::compiler::context::CompilationContext;
use crate::flatzinc::FlatZincError;

pub(crate) fn run(
    typed_ast: &Instance,
    context: &mut CompilationContext,
) -> Result<(), FlatZincError> {
    for (name, variable) in &typed_ast.variables {
        match &variable.domain.node {
            ast::Domain::Bool => {
                let (lb, ub) = match &variable.value {
                    None => (0, 1),

                    Some(ast::Node { node, .. }) => match node {
                        ast::Literal::Bool(true) => (1, 1),
                        ast::Literal::Bool(false) => (0, 0),

                        // The variable is assigned to another variable, but we don't handle that
                        // case here.
                        ast::Literal::Identifier(_) => (0, 1),

                        _ => panic!("expected boolean value, got {node:?}"),
                    },
                };

                context
                    .literal_equivalences
                    .create_equivalence_class(Rc::clone(name), lb, ub);
            }

            ast::Domain::Int(set) if set.is_continuous() => {
                let (lb, ub) = match &variable.value {
                    None => (*set.lower_bound(), *set.upper_bound()),

                    Some(ast::Node { node, .. }) => match node {
                        ast::Literal::Int(int) => (*int, *int),

                        // The variable is assigned to another variable, but we don't handle that
                        // case here.
                        ast::Literal::Identifier(_) => (0, 1),

                        _ => panic!("expected boolean value, got {node:?}"),
                    },
                };

                let lb = i32::try_from(lb)?;
                let ub = i32::try_from(ub)?;

                context
                    .integer_equivalences
                    .create_equivalence_class(Rc::clone(name), lb, ub);
            }

            ast::Domain::Int(set) => {
                assert!(!set.is_continuous());

                context
                    .integer_equivalences
                    .create_equivalence_class_sparse(
                        Rc::clone(name),
                        set.into_iter()
                            .map(i32::try_from)
                            .collect::<Result<Vec<i32>, _>>()?,
                    )
            }

            ast::Domain::UnboundedInt => {
                return Err(FlatZincError::UnsupportedVariable(name.as_ref().into()))
            }
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use fzn_rs::{Method, Solve};
    use pumpkin_solver::Solver;

    use super::*;
    use crate::flatzinc::{ast::VariableAnnotations, compiler::context::Domain};

    #[test]
    fn bool_variable_creates_equivalence_class() {
        let ast = create_dummy_instance([(
            "SomeVar",
            ast::Variable {
                domain: test_node(ast::Domain::Bool),
                value: None,
                annotations: vec![],
            },
        )]);

        let mut solver = Solver::default();
        let mut context = CompilationContext::new(&mut solver);

        run(&ast, &mut context).expect("no errors");

        let domain = context.literal_equivalences.domain("SomeVar");
        assert_eq!(Domain::from_lower_bound_and_upper_bound(0, 1), domain);
    }

    #[test]
    fn bool_variable_equal_to_constant_as_singleton_domain() {
        let ast = create_dummy_instance([
            (
                "SomeVar",
                ast::Variable {
                    domain: test_node(ast::Domain::Bool),
                    value: Some(test_node(ast::Literal::Bool(false))),
                    annotations: vec![],
                },
            ),
            (
                "OtherVar",
                ast::Variable {
                    domain: test_node(ast::Domain::Bool),
                    value: Some(test_node(ast::Literal::Bool(true))),
                    annotations: vec![],
                },
            ),
        ]);

        let mut solver = Solver::default();
        let mut context = CompilationContext::new(&mut solver);

        run(&ast, &mut context).expect("no errors");

        assert_eq!(
            Domain::from_lower_bound_and_upper_bound(0, 0),
            context.literal_equivalences.domain("SomeVar")
        );
        assert_eq!(
            Domain::from_lower_bound_and_upper_bound(1, 1),
            context.literal_equivalences.domain("OtherVar")
        );
    }

    #[test]
    fn bool_expr_ignores_reference_to_non_existent_identifier() {
        let ast = create_dummy_instance([(
            "SomeVar",
            ast::Variable {
                domain: test_node(ast::Domain::Bool),
                value: Some(test_node(ast::Literal::Identifier("OtherVar".into()))),
                annotations: vec![],
            },
        )]);

        let mut solver = Solver::default();
        let mut context = CompilationContext::new(&mut solver);

        run(&ast, &mut context).expect("no errors");

        assert_eq!(
            Domain::from_lower_bound_and_upper_bound(0, 1),
            context.literal_equivalences.domain("SomeVar")
        );
    }

    #[test]
    fn int_expr_constant_is_parsed() {
        let ast = create_dummy_instance([(
            "SomeVar",
            ast::Variable {
                domain: test_node(ast::Domain::Int(ast::RangeList::from(1..=5))),
                value: Some(test_node(ast::Literal::Int(3))),
                annotations: vec![],
            },
        )]);

        let mut solver = Solver::default();
        let mut context = CompilationContext::new(&mut solver);

        run(&ast, &mut context).expect("no errors");

        assert_eq!(
            Domain::from_lower_bound_and_upper_bound(3, 3),
            context.integer_equivalences.domain("SomeVar")
        );
    }

    fn create_dummy_instance(
        variables: impl IntoIterator<Item = (&'static str, ast::Variable<VariableAnnotations>)>,
    ) -> Instance {
        Instance {
            variables: variables
                .into_iter()
                .map(|(name, data)| (Rc::from(name), data))
                .collect(),
            constraints: vec![],
            solve: Solve {
                method: test_node(Method::Satisfy),
                annotations: vec![],
            },
        }
    }

    fn test_node<T>(data: T) -> ast::Node<T> {
        ast::Node {
            node: data,
            span: ast::Span { start: 0, end: 0 },
        }
    }
}
