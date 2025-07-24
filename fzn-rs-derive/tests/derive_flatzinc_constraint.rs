#![cfg(test)] // workaround for https://github.com/rust-lang/rust-clippy/issues/11024

mod utils;

use std::collections::BTreeMap;

use fzn_rs::ast::Argument;
use fzn_rs::ast::Array;
use fzn_rs::ast::Ast;
use fzn_rs::ast::Literal;
use fzn_rs::TypedInstance;
use fzn_rs::VariableExpr;
use fzn_rs_derive::FlatZincConstraint;
use utils::*;

#[test]
fn variant_with_unnamed_fields() {
    #[derive(Clone, Debug, PartialEq, Eq, FlatZincConstraint)]
    enum TypedConstraint {
        IntLinLe(Vec<i64>, Vec<VariableExpr<i64>>, i64),
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

    let instance = TypedInstance::<TypedConstraint>::from_ast(ast).expect("valid instance");

    assert_eq!(
        instance.constraints[0].constraint.node,
        TypedConstraint::IntLinLe(
            vec![2, 3, 5],
            vec![
                VariableExpr::Identifier("x1".into()),
                VariableExpr::Identifier("x2".into()),
                VariableExpr::Identifier("x3".into())
            ],
            3
        )
    )
}

#[test]
fn variant_with_named_fields() {
    #[derive(Clone, Debug, PartialEq, Eq, FlatZincConstraint)]
    enum TypedConstraint {
        IntLinLe {
            weights: Vec<i64>,
            variables: Vec<VariableExpr<i64>>,
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

    let instance = TypedInstance::<TypedConstraint>::from_ast(ast).expect("valid instance");

    assert_eq!(
        instance.constraints[0].constraint.node,
        TypedConstraint::IntLinLe {
            weights: vec![2, 3, 5],
            variables: vec![
                VariableExpr::Identifier("x1".into()),
                VariableExpr::Identifier("x2".into()),
                VariableExpr::Identifier("x3".into())
            ],
            bound: 3
        }
    )
}

#[test]
fn variant_with_name_attribute() {
    #[derive(Clone, Debug, PartialEq, Eq, FlatZincConstraint)]
    enum TypedConstraint {
        #[name("int_lin_le")]
        LinearInequality {
            weights: Vec<i64>,
            variables: Vec<VariableExpr<i64>>,
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

    let instance = TypedInstance::<TypedConstraint>::from_ast(ast).expect("valid instance");

    assert_eq!(
        instance.constraints[0].constraint.node,
        TypedConstraint::LinearInequality {
            weights: vec![2, 3, 5],
            variables: vec![
                VariableExpr::Identifier("x1".into()),
                VariableExpr::Identifier("x2".into()),
                VariableExpr::Identifier("x3".into())
            ],
            bound: 3
        }
    )
}

#[test]
fn constraint_referencing_arrays() {
    #[derive(Clone, Debug, PartialEq, Eq, FlatZincConstraint)]
    enum TypedConstraint {
        IntLinLe(Vec<i64>, Vec<VariableExpr<i64>>, i64),
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

    let instance = TypedInstance::<TypedConstraint>::from_ast(ast).expect("valid instance");

    assert_eq!(
        instance.constraints[0].constraint.node,
        TypedConstraint::IntLinLe(
            vec![2, 3, 5],
            vec![
                VariableExpr::Identifier("x1".into()),
                VariableExpr::Identifier("x2".into()),
                VariableExpr::Identifier("x3".into())
            ],
            3
        )
    )
}

#[test]
fn constraint_as_struct_args() {
    #[derive(Clone, Debug, PartialEq, Eq, FlatZincConstraint)]
    enum TypedConstraint {
        #[args]
        IntLinLe(LinearLeq),
    }

    #[derive(Clone, Debug, PartialEq, Eq, FlatZincConstraint)]
    struct LinearLeq {
        weights: Vec<i64>,
        variables: Vec<VariableExpr<i64>>,
        bound: i64,
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

    let instance = TypedInstance::<TypedConstraint>::from_ast(ast).expect("valid instance");

    assert_eq!(
        instance.constraints[0].constraint.node,
        TypedConstraint::IntLinLe(LinearLeq {
            weights: vec![2, 3, 5],
            variables: vec![
                VariableExpr::Identifier("x1".into()),
                VariableExpr::Identifier("x2".into()),
                VariableExpr::Identifier("x3".into())
            ],
            bound: 3,
        })
    )
}
