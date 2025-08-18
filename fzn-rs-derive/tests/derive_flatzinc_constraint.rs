#![cfg(test)] // workaround for https://github.com/rust-lang/rust-clippy/issues/11024

mod utils;

use std::collections::BTreeMap;

use fzn_rs::ast::Argument;
use fzn_rs::ast::Array;
use fzn_rs::ast::Ast;
use fzn_rs::ast::Literal;
use fzn_rs::ast::Span;
use fzn_rs::ArrayExpr;
use fzn_rs::InstanceError;
use fzn_rs::TypedInstance;
use fzn_rs::VariableExpr;
use fzn_rs_derive::FlatZincConstraint;
use utils::*;

#[test]
fn variant_with_unnamed_fields() {
    #[derive(Clone, Debug, PartialEq, Eq, FlatZincConstraint)]
    enum TypedConstraint {
        IntLinLe(ArrayExpr<i64>, ArrayExpr<VariableExpr<i64>>, i64),
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

    let instance = TypedInstance::<i64, TypedConstraint>::from_ast(ast).expect("valid instance");
    let TypedConstraint::IntLinLe(weights, variables, bound) =
        instance.constraints[0].clone().constraint.node;

    let weights = instance
        .resolve_array(&weights)
        .unwrap()
        .collect::<Result<Vec<_>, _>>()
        .unwrap();
    let variables = instance
        .resolve_array(&variables)
        .unwrap()
        .collect::<Result<Vec<_>, _>>()
        .unwrap();

    assert_eq!(weights, vec![2, 3, 5]);
    assert_eq!(
        variables,
        vec![
            VariableExpr::Identifier("x1".into()),
            VariableExpr::Identifier("x2".into()),
            VariableExpr::Identifier("x3".into())
        ]
    );
    assert_eq!(bound, 3);
}

#[test]
fn variant_with_named_fields() {
    #[derive(Clone, Debug, PartialEq, Eq, FlatZincConstraint)]
    enum TypedConstraint {
        IntLinLe {
            weights: ArrayExpr<i64>,
            variables: ArrayExpr<VariableExpr<i64>>,
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

    let instance = TypedInstance::<i64, TypedConstraint>::from_ast(ast).expect("valid instance");
    let TypedConstraint::IntLinLe {
        weights,
        variables,
        bound,
    } = instance.constraints[0].clone().constraint.node;

    let weights = instance
        .resolve_array(&weights)
        .unwrap()
        .collect::<Result<Vec<_>, _>>()
        .unwrap();
    let variables = instance
        .resolve_array(&variables)
        .unwrap()
        .collect::<Result<Vec<_>, _>>()
        .unwrap();

    assert_eq!(weights, vec![2, 3, 5]);
    assert_eq!(
        variables,
        vec![
            VariableExpr::Identifier("x1".into()),
            VariableExpr::Identifier("x2".into()),
            VariableExpr::Identifier("x3".into())
        ]
    );
    assert_eq!(bound, 3);
}

#[test]
fn variant_with_name_attribute() {
    #[derive(Clone, Debug, PartialEq, Eq, FlatZincConstraint)]
    enum TypedConstraint {
        #[name("int_lin_le")]
        LinearInequality {
            weights: ArrayExpr<i64>,
            variables: ArrayExpr<VariableExpr<i64>>,
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

    let instance = TypedInstance::<i64, TypedConstraint>::from_ast(ast).expect("valid instance");
    let TypedConstraint::LinearInequality {
        weights,
        variables,
        bound,
    } = instance.constraints[0].clone().constraint.node;

    let weights = instance
        .resolve_array(&weights)
        .unwrap()
        .collect::<Result<Vec<_>, _>>()
        .unwrap();
    let variables = instance
        .resolve_array(&variables)
        .unwrap()
        .collect::<Result<Vec<_>, _>>()
        .unwrap();

    assert_eq!(weights, vec![2, 3, 5]);
    assert_eq!(
        variables,
        vec![
            VariableExpr::Identifier("x1".into()),
            VariableExpr::Identifier("x2".into()),
            VariableExpr::Identifier("x3".into())
        ]
    );
    assert_eq!(bound, 3);
}

#[test]
fn constraint_referencing_arrays() {
    #[derive(Clone, Debug, PartialEq, Eq, FlatZincConstraint)]
    enum TypedConstraint {
        IntLinLe(ArrayExpr<i64>, ArrayExpr<VariableExpr<i64>>, i64),
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

    let instance = TypedInstance::<i64, TypedConstraint>::from_ast(ast).expect("valid instance");

    let TypedConstraint::IntLinLe(weights, variables, bound) =
        instance.constraints[0].clone().constraint.node;

    let weights = instance
        .resolve_array(&weights)
        .unwrap()
        .collect::<Result<Vec<_>, _>>()
        .unwrap();
    let variables = instance
        .resolve_array(&variables)
        .unwrap()
        .collect::<Result<Vec<_>, _>>()
        .unwrap();

    assert_eq!(weights, vec![2, 3, 5]);
    assert_eq!(
        variables,
        vec![
            VariableExpr::Identifier("x1".into()),
            VariableExpr::Identifier("x2".into()),
            VariableExpr::Identifier("x3".into())
        ]
    );
    assert_eq!(bound, 3);
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
        weights: ArrayExpr<i64>,
        variables: ArrayExpr<VariableExpr<i64>>,
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

    let instance = TypedInstance::<i64, TypedConstraint>::from_ast(ast).expect("valid instance");

    let TypedConstraint::IntLinLe(linear) = instance.constraints[0].clone().constraint.node;

    let weights = instance
        .resolve_array(&linear.weights)
        .unwrap()
        .collect::<Result<Vec<_>, _>>()
        .unwrap();
    let variables = instance
        .resolve_array(&linear.variables)
        .unwrap()
        .collect::<Result<Vec<_>, _>>()
        .unwrap();

    assert_eq!(weights, vec![2, 3, 5]);
    assert_eq!(
        variables,
        vec![
            VariableExpr::Identifier("x1".into()),
            VariableExpr::Identifier("x2".into()),
            VariableExpr::Identifier("x3".into())
        ]
    );
    assert_eq!(linear.bound, 3);
}

#[test]
fn argument_count_on_tuple_variants() {
    #[derive(Clone, Debug, PartialEq, Eq, FlatZincConstraint)]
    enum TypedConstraint {
        SomeConstraint(i64),
    }

    let ast = Ast {
        variables: BTreeMap::new(),
        arrays: BTreeMap::new(),
        constraints: vec![node(
            fzn_rs::ast::Constraint {
                name: test_node("some_constraint".into()),
                arguments: vec![
                    test_node(Argument::Literal(test_node(Literal::Int(3)))),
                    test_node(Argument::Literal(test_node(Literal::Int(3)))),
                ],
                annotations: vec![],
            },
            0,
            10,
        )],
        solve: satisfy_solve(),
    };

    let error = TypedInstance::<i64, TypedConstraint>::from_ast(ast).expect_err("invalid instance");

    assert_eq!(
        error,
        InstanceError::IncorrectNumberOfArguments {
            expected: 1,
            actual: 2,
            span: Span { start: 0, end: 10 },
        }
    );
}

#[test]
fn argument_count_on_named_fields_variant() {
    #[derive(Clone, Debug, PartialEq, Eq, FlatZincConstraint)]
    enum TypedConstraint {
        SomeConstraint { constant: i64 },
    }

    let ast = Ast {
        variables: BTreeMap::new(),
        arrays: BTreeMap::new(),
        constraints: vec![node(
            fzn_rs::ast::Constraint {
                name: test_node("some_constraint".into()),
                arguments: vec![
                    test_node(Argument::Literal(test_node(Literal::Int(3)))),
                    test_node(Argument::Literal(test_node(Literal::Int(3)))),
                ],
                annotations: vec![],
            },
            0,
            10,
        )],
        solve: satisfy_solve(),
    };

    let error = TypedInstance::<i64, TypedConstraint>::from_ast(ast).expect_err("invalid instance");

    assert_eq!(
        error,
        InstanceError::IncorrectNumberOfArguments {
            expected: 1,
            actual: 2,
            span: Span { start: 0, end: 10 },
        }
    );
}

#[test]
fn argument_count_on_args_struct() {
    #[derive(Clone, Debug, PartialEq, Eq, FlatZincConstraint)]
    enum TypedConstraint {
        #[args]
        SomeConstraint(Args),
    }

    #[derive(Clone, Debug, PartialEq, Eq, FlatZincConstraint)]
    struct Args {
        argument: i64,
    }

    let ast = Ast {
        variables: BTreeMap::new(),
        arrays: BTreeMap::new(),
        constraints: vec![node(
            fzn_rs::ast::Constraint {
                name: test_node("some_constraint".into()),
                arguments: vec![
                    test_node(Argument::Literal(test_node(Literal::Int(3)))),
                    test_node(Argument::Literal(test_node(Literal::Int(3)))),
                ],
                annotations: vec![],
            },
            0,
            10,
        )],
        solve: satisfy_solve(),
    };

    let error = TypedInstance::<i64, TypedConstraint>::from_ast(ast).expect_err("invalid instance");

    assert_eq!(
        error,
        InstanceError::IncorrectNumberOfArguments {
            expected: 1,
            actual: 2,
            span: Span { start: 0, end: 10 },
        }
    );
}
