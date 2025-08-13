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
use fzn_rs::ast::Domain;
use fzn_rs::ast::Literal;
use fzn_rs::ast::RangeList;
use fzn_rs::ast::Variable;
use fzn_rs::ArrayExpr;
use fzn_rs::TypedInstance;
use fzn_rs::VariableExpr;
use fzn_rs_derive::FlatZincAnnotation;
use fzn_rs_derive::FlatZincConstraint;
use utils::*;

macro_rules! btreemap {
    ($($key:expr => $value:expr,)+) => (btreemap!($($key => $value),+));

    ( $($key:expr => $value:expr),* ) => {
        {
            let mut _map = ::std::collections::BTreeMap::new();
            $(
                let _ = _map.insert($key, $value);
            )*
            _map
        }
    };
}

#[test]
fn annotation_without_arguments() {
    #[derive(Clone, Debug, PartialEq, Eq, FlatZincConstraint)]
    enum TypedConstraint {
        SomeConstraint(VariableExpr<i64>),
    }

    #[derive(Clone, Debug, PartialEq, Eq, FlatZincAnnotation)]
    enum TypedAnnotation {
        OutputVar,
    }

    type Instance = TypedInstance<i64, TypedConstraint, (), (), TypedAnnotation, ()>;

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

    let instance = Instance::from_ast(ast).expect("valid instance");

    assert_eq!(
        instance.constraints[0].annotations[0].node,
        TypedAnnotation::OutputVar,
    );
}

#[test]
fn annotation_with_positional_literal_arguments() {
    #[derive(Clone, Debug, PartialEq, Eq, FlatZincConstraint)]
    enum TypedConstraint {
        SomeConstraint(VariableExpr<i64>),
    }

    #[derive(Clone, Debug, PartialEq, Eq, FlatZincAnnotation)]
    enum TypedAnnotation {
        DefinesVar(Rc<str>),
        OutputArray(RangeList<i64>),
    }

    type Instance = TypedInstance<i64, TypedConstraint, (), (), TypedAnnotation, ()>;

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

    let instance = Instance::from_ast(ast).expect("valid instance");

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
    enum TypedConstraint {
        SomeConstraint(VariableExpr<i64>),
    }

    #[derive(Clone, Debug, PartialEq, Eq, FlatZincAnnotation)]
    enum TypedAnnotation {
        DefinesVar { variable_id: Rc<str> },
    }

    type Instance = TypedInstance<i64, TypedConstraint, (), (), TypedAnnotation, ()>;

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

    let instance = Instance::from_ast(ast).expect("valid instance");

    assert_eq!(
        instance.constraints[0].annotations[0].node,
        TypedAnnotation::DefinesVar {
            variable_id: "some_var".into()
        },
    );
}

#[test]
fn nested_annotation_as_argument() {
    #[derive(Clone, Debug, PartialEq, Eq, FlatZincConstraint)]
    enum TypedConstraint {
        SomeConstraint(VariableExpr<i64>),
    }

    #[derive(Clone, Debug, PartialEq, Eq, FlatZincAnnotation)]
    enum TypedAnnotation {
        SomeAnnotation(#[annotation] SomeAnnotationArgs),
    }

    #[derive(Clone, Debug, PartialEq, Eq, FlatZincAnnotation)]
    enum SomeAnnotationArgs {
        ArgOne,
        ArgTwo(Rc<str>),
    }

    type Instance = TypedInstance<i64, TypedConstraint, (), (), TypedAnnotation, ()>;

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
                    name: "some_annotation".into(),
                    arguments: vec![test_node(AnnotationArgument::Literal(test_node(
                        AnnotationLiteral::BaseLiteral(Literal::Identifier("arg_one".into())),
                    )))],
                })),
                test_node(Annotation::Call(AnnotationCall {
                    name: "some_annotation".into(),
                    arguments: vec![test_node(AnnotationArgument::Literal(test_node(
                        AnnotationLiteral::Annotation(AnnotationCall {
                            name: "arg_two".into(),
                            arguments: vec![test_node(AnnotationArgument::Literal(test_node(
                                AnnotationLiteral::BaseLiteral(Literal::Identifier("ident".into())),
                            )))],
                        }),
                    )))],
                })),
            ],
        })],

        solve: satisfy_solve(),
    };

    let instance = Instance::from_ast(ast).expect("valid instance");

    assert_eq!(
        instance.constraints[0].annotations[0].node,
        TypedAnnotation::SomeAnnotation(SomeAnnotationArgs::ArgOne),
    );

    assert_eq!(
        instance.constraints[0].annotations[1].node,
        TypedAnnotation::SomeAnnotation(SomeAnnotationArgs::ArgTwo("ident".into())),
    );
}

#[test]
fn arrays_as_annotation_arguments_with_literal_elements() {
    #[derive(Clone, Debug, PartialEq, Eq, FlatZincConstraint)]
    enum TypedConstraint {
        SomeConstraint(VariableExpr<i64>),
    }

    #[derive(Clone, Debug, PartialEq, Eq, FlatZincAnnotation)]
    enum TypedAnnotation {
        SomeAnnotation(ArrayExpr<i64>),
    }

    type Instance = TypedInstance<i64, TypedConstraint, (), (), TypedAnnotation, ()>;

    let ast = Ast {
        variables: BTreeMap::new(),
        arrays: BTreeMap::new(),
        constraints: vec![test_node(fzn_rs::ast::Constraint {
            name: test_node("some_constraint".into()),
            arguments: vec![test_node(Argument::Literal(test_node(
                Literal::Identifier("x3".into()),
            )))],
            annotations: vec![test_node(Annotation::Call(AnnotationCall {
                name: "some_annotation".into(),
                arguments: vec![test_node(AnnotationArgument::Array(vec![
                    test_node(AnnotationLiteral::BaseLiteral(Literal::Int(1))),
                    test_node(AnnotationLiteral::BaseLiteral(Literal::Int(2))),
                ]))],
            }))],
        })],

        solve: satisfy_solve(),
    };

    let instance = Instance::from_ast(ast).expect("valid instance");
    let TypedAnnotation::SomeAnnotation(args) = instance.constraints[0].annotations[0].node.clone();

    let resolved_args = instance
        .resolve_array(&args)
        .unwrap()
        .collect::<Result<Vec<_>, _>>()
        .unwrap();

    assert_eq!(resolved_args, vec![1, 2]);
}

#[test]
fn arrays_as_annotation_arguments_with_annotation_elements() {
    #[derive(Clone, Debug, PartialEq, Eq, FlatZincConstraint)]
    enum TypedConstraint {
        SomeConstraint(VariableExpr<i64>),
    }

    #[derive(Clone, Debug, PartialEq, Eq, FlatZincAnnotation)]
    enum TypedAnnotation {
        SomeAnnotation(#[annotation] Vec<ArrayElements>),
    }

    #[derive(Clone, Debug, PartialEq, Eq, FlatZincAnnotation)]
    enum ArrayElements {
        ElementOne,
        ElementTwo(i64),
    }

    type Instance = TypedInstance<i64, TypedConstraint, (), (), TypedAnnotation, ()>;

    let ast = Ast {
        variables: BTreeMap::new(),
        arrays: BTreeMap::new(),
        constraints: vec![test_node(fzn_rs::ast::Constraint {
            name: test_node("some_constraint".into()),
            arguments: vec![test_node(Argument::Literal(test_node(
                Literal::Identifier("x3".into()),
            )))],
            annotations: vec![test_node(Annotation::Call(AnnotationCall {
                name: "some_annotation".into(),
                arguments: vec![test_node(AnnotationArgument::Array(vec![
                    test_node(AnnotationLiteral::BaseLiteral(Literal::Identifier(
                        "element_one".into(),
                    ))),
                    test_node(AnnotationLiteral::Annotation(AnnotationCall {
                        name: "element_two".into(),
                        arguments: vec![test_node(AnnotationArgument::Literal(test_node(
                            AnnotationLiteral::BaseLiteral(Literal::Int(4)),
                        )))],
                    })),
                ]))],
            }))],
        })],

        solve: satisfy_solve(),
    };

    let instance = Instance::from_ast(ast).expect("valid instance");

    assert_eq!(
        instance.constraints[0].annotations[0].node,
        TypedAnnotation::SomeAnnotation(vec![
            ArrayElements::ElementOne,
            ArrayElements::ElementTwo(4)
        ]),
    );
}

#[test]
fn annotations_can_be_structs_for_arguments() {
    #[derive(Clone, Debug, PartialEq, Eq, FlatZincConstraint)]
    enum TypedConstraint {
        SomeConstraint(VariableExpr<i64>),
    }

    #[derive(Clone, Debug, PartialEq, Eq, FlatZincAnnotation)]
    enum TypedAnnotation {
        #[args]
        SomeAnnotation(AnnotationArgs),
    }

    #[derive(Clone, Debug, PartialEq, Eq, FlatZincAnnotation)]
    struct AnnotationArgs {
        ident: Rc<str>,
        #[annotation]
        ann: OtherAnnotation,
    }

    #[derive(Clone, Debug, PartialEq, Eq, FlatZincAnnotation)]
    enum OtherAnnotation {
        ElementOne,
        ElementTwo(i64),
    }

    type Instance = TypedInstance<i64, TypedConstraint, TypedAnnotation>;

    let ast = Ast {
        variables: btreemap! {
            "x1".into() => test_node(Variable {
                domain: test_node(Domain::UnboundedInt),
                value: None,
                annotations: vec![test_node(Annotation::Call(AnnotationCall {
                    name: "some_annotation".into(),
                    arguments: vec![
                        test_node(AnnotationArgument::Literal(test_node(AnnotationLiteral::BaseLiteral(Literal::Identifier("some_ident".into()))))),
                        test_node(AnnotationArgument::Literal(test_node(AnnotationLiteral::BaseLiteral(Literal::Identifier("element_one".into()))))),
                    ],
                }))],
            }),
        },
        arrays: BTreeMap::new(),
        constraints: vec![],
        solve: satisfy_solve(),
    };

    let instance = Instance::from_ast(ast).expect("valid instance");

    assert_eq!(
        instance.variables["x1"].annotations[0].node,
        TypedAnnotation::SomeAnnotation(AnnotationArgs {
            ident: "some_ident".into(),
            ann: OtherAnnotation::ElementOne,
        }),
    );
}
