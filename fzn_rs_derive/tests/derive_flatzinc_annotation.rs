#![cfg(test)] // workaround for https://github.com/rust-lang/rust-clippy/issues/11024

mod utils;

use std::collections::BTreeMap;
use std::rc::Rc;

use fzn_rs::ast::Annotation;
use fzn_rs::ast::AnnotationArgument;
use fzn_rs::ast::AnnotationCall;
use fzn_rs::ast::AnnotationLiteral;
use fzn_rs::ast::Argument;
use fzn_rs::ast::Ast;
use fzn_rs::ast::Literal;
use fzn_rs::ast::RangeList;
use fzn_rs::Instance;
use fzn_rs::VariableArgument;
use fzn_rs_derive::FlatZincAnnotation;
use fzn_rs_derive::FlatZincConstraint;
use utils::*;

#[test]
fn annotation_without_arguments() {
    #[derive(Clone, Debug, PartialEq, Eq, FlatZincConstraint)]
    enum InstanceConstraint {
        SomeConstraint(VariableArgument<i64>),
    }

    #[derive(Clone, Debug, PartialEq, Eq, FlatZincAnnotation)]
    enum TypedAnnotation {
        OutputVar,
    }

    let ast = Ast {
        variables: BTreeMap::new(),
        arrays: BTreeMap::new(),
        constraints: vec![test_node(fzn_rs::ast::Constraint {
            name: test_node("some_constraint".into()),
            arguments: vec![test_node(Argument::Literal(test_node(
                Literal::Identifier("x3".into()),
            )))],
            annotations: vec![test_node(Annotation::Atom("output_var".into()))],
        })],
        solve: satisfy_solve(),
    };

    let instance =
        Instance::<InstanceConstraint, TypedAnnotation>::from_ast(ast).expect("valid instance");

    assert_eq!(
        instance.constraints[0].annotations[0].node,
        TypedAnnotation::OutputVar,
    );
}

#[test]
fn annotation_with_positional_literal_arguments() {
    #[derive(Clone, Debug, PartialEq, Eq, FlatZincConstraint)]
    enum InstanceConstraint {
        SomeConstraint(VariableArgument<i64>),
    }

    #[derive(Clone, Debug, PartialEq, Eq, FlatZincAnnotation)]
    enum TypedAnnotation {
        DefinesVar(Rc<str>),
        OutputArray(RangeList<i64>),
    }

    let ast = Ast {
        variables: BTreeMap::new(),
        arrays: BTreeMap::new(),
        constraints: vec![test_node(fzn_rs::ast::Constraint {
            name: test_node("some_constraint".into()),
            arguments: vec![test_node(Argument::Literal(test_node(
                Literal::Identifier("x3".into()),
            )))],
            annotations: vec![
                test_node(Annotation::Call(AnnotationCall {
                    name: "defines_var".into(),
                    arguments: vec![test_node(AnnotationArgument::Literal(test_node(
                        AnnotationLiteral::BaseLiteral(Literal::Identifier("some_var".into())),
                    )))],
                })),
                test_node(Annotation::Call(AnnotationCall {
                    name: "output_array".into(),
                    arguments: vec![test_node(AnnotationArgument::Literal(test_node(
                        AnnotationLiteral::BaseLiteral(Literal::IntSet(RangeList::from(1..=5))),
                    )))],
                })),
            ],
        })],

        solve: satisfy_solve(),
    };

    let instance =
        Instance::<InstanceConstraint, TypedAnnotation>::from_ast(ast).expect("valid instance");

    assert_eq!(
        instance.constraints[0].annotations[0].node,
        TypedAnnotation::DefinesVar("some_var".into()),
    );

    assert_eq!(
        instance.constraints[0].annotations[1].node,
        TypedAnnotation::OutputArray(RangeList::from(1..=5)),
    );
}

#[test]
fn annotation_with_named_arguments() {
    #[derive(Clone, Debug, PartialEq, Eq, FlatZincConstraint)]
    enum InstanceConstraint {
        SomeConstraint(VariableArgument<i64>),
    }

    #[derive(Clone, Debug, PartialEq, Eq, FlatZincAnnotation)]
    enum TypedAnnotation {
        DefinesVar { variable_id: Rc<str> },
    }

    let ast = Ast {
        variables: BTreeMap::new(),
        arrays: BTreeMap::new(),
        constraints: vec![test_node(fzn_rs::ast::Constraint {
            name: test_node("some_constraint".into()),
            arguments: vec![test_node(Argument::Literal(test_node(
                Literal::Identifier("x3".into()),
            )))],
            annotations: vec![test_node(Annotation::Call(AnnotationCall {
                name: "defines_var".into(),
                arguments: vec![test_node(AnnotationArgument::Literal(test_node(
                    AnnotationLiteral::BaseLiteral(Literal::Identifier("some_var".into())),
                )))],
            }))],
        })],

        solve: satisfy_solve(),
    };

    let instance =
        Instance::<InstanceConstraint, TypedAnnotation>::from_ast(ast).expect("valid instance");

    assert_eq!(
        instance.constraints[0].annotations[0].node,
        TypedAnnotation::DefinesVar {
            variable_id: "some_var".into()
        },
    );
}
