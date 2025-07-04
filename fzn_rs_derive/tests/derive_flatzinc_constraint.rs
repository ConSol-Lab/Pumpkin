#![cfg(test)] // workaround for https://github.com/rust-lang/rust-clippy/issues/11024

use std::collections::BTreeMap;
use std::rc::Rc;

use fzn_rs::ast::Annotation;
use fzn_rs::ast::Argument;
use fzn_rs::ast::Array;
use fzn_rs::ast::Ast;
use fzn_rs::ast::Domain;
use fzn_rs::ast::Literal;
use fzn_rs::ast::Node;
use fzn_rs::ast::SolveObjective;
use fzn_rs::ast::Span;
use fzn_rs::ast::Variable;
use fzn_rs::Instance;
use fzn_rs::IntVariable;
use fzn_rs_derive::FlatZincConstraint;

fn satisfy_solve() -> SolveObjective {
    SolveObjective {
        method: fzn_rs::ast::Method::Satisfy,
        annotations: vec![],
    }
}

fn test_node<T>(node: T) -> Node<T> {
    Node {
        node,
        span: Span { start: 0, end: 0 },
    }
}

fn unbounded_variables<'a>(
    names: impl IntoIterator<Item = &'a str>,
) -> BTreeMap<Rc<str>, Node<Variable<Annotation>>> {
    names
        .into_iter()
        .map(|name| {
            (
                Rc::from(name),
                test_node(Variable {
                    domain: test_node(Domain::UnboundedInt),
                    value: None,
                    annotations: vec![],
                }),
            )
        })
        .collect()
}

#[test]
fn variant_with_unnamed_fields() {
    #[derive(Clone, Debug, PartialEq, Eq, FlatZincConstraint)]
    enum InstanceConstraint {
        IntLinLe(Vec<i64>, Vec<IntVariable>, i64),
    }

    let ast = Ast {
        variables: unbounded_variables(["x1", "x2", "x3"]),
        arrays: BTreeMap::new(),
        constraints: vec![test_node(fzn_rs::ast::Constraint {
            name: test_node("int_lin_le".into()),
            arguments: vec![
                test_node(Argument::Array(vec![
                    test_node(Literal::Int(2)),
                    test_node(Literal::Int(3)),
                    test_node(Literal::Int(5)),
                ])),
                test_node(Argument::Array(vec![
                    test_node(Literal::Identifier("x1".into())),
                    test_node(Literal::Identifier("x2".into())),
                    test_node(Literal::Identifier("x3".into())),
                ])),
                test_node(Argument::Literal(test_node(Literal::Int(3)))),
            ],
            annotations: vec![],
        })],
        solve: satisfy_solve(),
    };

    let instance = Instance::<InstanceConstraint>::from_ast(ast).expect("valid instance");

    assert_eq!(
        instance.constraints[0].constraint.node,
        InstanceConstraint::IntLinLe(
            vec![2, 3, 5],
            vec![
                IntVariable::Identifier("x1".into()),
                IntVariable::Identifier("x2".into()),
                IntVariable::Identifier("x3".into())
            ],
            3
        )
    )
}

#[test]
fn variant_with_named_fields() {
    #[derive(Clone, Debug, PartialEq, Eq, FlatZincConstraint)]
    enum InstanceConstraint {
        IntLinLe {
            weights: Vec<i64>,
            variables: Vec<IntVariable>,
            bound: i64,
        },
    }

    let ast = Ast {
        variables: unbounded_variables(["x1", "x2", "x3"]),
        arrays: BTreeMap::new(),
        constraints: vec![test_node(fzn_rs::ast::Constraint {
            name: test_node("int_lin_le".into()),
            arguments: vec![
                test_node(Argument::Array(vec![
                    test_node(Literal::Int(2)),
                    test_node(Literal::Int(3)),
                    test_node(Literal::Int(5)),
                ])),
                test_node(Argument::Array(vec![
                    test_node(Literal::Identifier("x1".into())),
                    test_node(Literal::Identifier("x2".into())),
                    test_node(Literal::Identifier("x3".into())),
                ])),
                test_node(Argument::Literal(test_node(Literal::Int(3)))),
            ],
            annotations: vec![],
        })],
        solve: satisfy_solve(),
    };

    let instance = Instance::<InstanceConstraint>::from_ast(ast).expect("valid instance");

    assert_eq!(
        instance.constraints[0].constraint.node,
        InstanceConstraint::IntLinLe {
            weights: vec![2, 3, 5],
            variables: vec![
                IntVariable::Identifier("x1".into()),
                IntVariable::Identifier("x2".into()),
                IntVariable::Identifier("x3".into())
            ],
            bound: 3
        }
    )
}

#[test]
fn variant_with_name_attribute() {
    #[derive(Clone, Debug, PartialEq, Eq, FlatZincConstraint)]
    enum InstanceConstraint {
        #[name("int_lin_le")]
        LinearInequality {
            weights: Vec<i64>,
            variables: Vec<IntVariable>,
            bound: i64,
        },
    }

    let ast = Ast {
        variables: unbounded_variables(["x1", "x2", "x3"]),
        arrays: BTreeMap::new(),
        constraints: vec![test_node(fzn_rs::ast::Constraint {
            name: test_node("int_lin_le".into()),
            arguments: vec![
                test_node(Argument::Array(vec![
                    test_node(Literal::Int(2)),
                    test_node(Literal::Int(3)),
                    test_node(Literal::Int(5)),
                ])),
                test_node(Argument::Array(vec![
                    test_node(Literal::Identifier("x1".into())),
                    test_node(Literal::Identifier("x2".into())),
                    test_node(Literal::Identifier("x3".into())),
                ])),
                test_node(Argument::Literal(test_node(Literal::Int(3)))),
            ],
            annotations: vec![],
        })],
        solve: satisfy_solve(),
    };

    let instance = Instance::<InstanceConstraint>::from_ast(ast).expect("valid instance");

    assert_eq!(
        instance.constraints[0].constraint.node,
        InstanceConstraint::LinearInequality {
            weights: vec![2, 3, 5],
            variables: vec![
                IntVariable::Identifier("x1".into()),
                IntVariable::Identifier("x2".into()),
                IntVariable::Identifier("x3".into())
            ],
            bound: 3
        }
    )
}

#[test]
fn constraint_referencing_arrays() {
    #[derive(Clone, Debug, PartialEq, Eq, FlatZincConstraint)]
    enum InstanceConstraint {
        IntLinLe(Vec<i64>, Vec<IntVariable>, i64),
    }

    let ast = Ast {
        variables: unbounded_variables(["x1", "x2", "x3"]),
        arrays: [
            (
                "array1".into(),
                test_node(Array {
                    contents: vec![
                        test_node(Literal::Int(2)),
                        test_node(Literal::Int(3)),
                        test_node(Literal::Int(5)),
                    ],
                    annotations: vec![],
                }),
            ),
            (
                "array2".into(),
                test_node(Array {
                    contents: vec![
                        test_node(Literal::Identifier("x1".into())),
                        test_node(Literal::Identifier("x2".into())),
                        test_node(Literal::Identifier("x3".into())),
                    ],
                    annotations: vec![],
                }),
            ),
        ]
        .into_iter()
        .collect(),
        constraints: vec![test_node(fzn_rs::ast::Constraint {
            name: test_node("int_lin_le".into()),
            arguments: vec![
                test_node(Argument::Literal(test_node(Literal::Identifier(
                    "array1".into(),
                )))),
                test_node(Argument::Literal(test_node(Literal::Identifier(
                    "array2".into(),
                )))),
                test_node(Argument::Literal(test_node(Literal::Int(3)))),
            ],
            annotations: vec![],
        })],
        solve: satisfy_solve(),
    };

    let instance = Instance::<InstanceConstraint>::from_ast(ast).expect("valid instance");

    assert_eq!(
        instance.constraints[0].constraint.node,
        InstanceConstraint::IntLinLe(
            vec![2, 3, 5],
            vec![
                IntVariable::Identifier("x1".into()),
                IntVariable::Identifier("x2".into()),
                IntVariable::Identifier("x3".into())
            ],
            3
        )
    )
}
