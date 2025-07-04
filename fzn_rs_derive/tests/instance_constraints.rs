#![cfg(test)] // workaround for https://github.com/rust-lang/rust-clippy/issues/11024

use std::collections::BTreeMap;
use std::rc::Rc;

use fzn_rs::ast::Annotation;
use fzn_rs::ast::Argument;
use fzn_rs::ast::Ast;
use fzn_rs::ast::Domain;
use fzn_rs::ast::Literal;
use fzn_rs::ast::SolveObjective;
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

fn unbounded_int_variable(name: &str) -> (Rc<str>, Variable<Annotation>) {
    (
        name.into(),
        Variable {
            domain: Domain::UnboundedInt,
            value: None,
            annotations: vec![],
        },
    )
}

#[test]
fn variant_with_unnamed_fields() {
    #[derive(Clone, Debug, PartialEq, Eq, FlatZincConstraint)]
    enum InstanceConstraint {
        IntLinLe(Vec<i64>, Vec<IntVariable>, i64),
    }

    let ast = Ast {
        variables: vec![
            unbounded_int_variable("x1"),
            unbounded_int_variable("x2"),
            unbounded_int_variable("x3"),
        ]
        .into_iter()
        .collect(),
        arrays: BTreeMap::new(),
        constraints: vec![fzn_rs::ast::Constraint {
            name: "int_lin_le".into(),
            arguments: vec![
                Argument::Array(vec![Literal::Int(2), Literal::Int(3), Literal::Int(5)]),
                Argument::Array(vec![
                    Literal::Identifier("x1".into()),
                    Literal::Identifier("x2".into()),
                    Literal::Identifier("x3".into()),
                ]),
                Argument::Literal(Literal::Int(3)),
            ],
            annotations: vec![],
        }],
        solve: satisfy_solve(),
    };

    let instance = Instance::<InstanceConstraint>::from_ast(ast).expect("valid instance");

    assert_eq!(
        instance.constraints[0].constraint,
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
        variables: vec![
            unbounded_int_variable("x1"),
            unbounded_int_variable("x2"),
            unbounded_int_variable("x3"),
        ]
        .into_iter()
        .collect(),
        arrays: BTreeMap::new(),
        constraints: vec![fzn_rs::ast::Constraint {
            name: "int_lin_le".into(),
            arguments: vec![
                Argument::Array(vec![Literal::Int(2), Literal::Int(3), Literal::Int(5)]),
                Argument::Array(vec![
                    Literal::Identifier("x1".into()),
                    Literal::Identifier("x2".into()),
                    Literal::Identifier("x3".into()),
                ]),
                Argument::Literal(Literal::Int(3)),
            ],
            annotations: vec![],
        }],
        solve: satisfy_solve(),
    };

    let instance = Instance::<InstanceConstraint>::from_ast(ast).expect("valid instance");

    assert_eq!(
        instance.constraints[0].constraint,
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
