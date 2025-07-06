//! This module contains traits that help with extracting a [`crate::Instance`] from an
//! [`crate::ast::Ast`].

use std::collections::BTreeMap;
use std::rc::Rc;

use crate::ast::RangeList;
use crate::ast::{self};
use crate::error::Token;
use crate::InstanceError;

/// Models a variable in the FlatZinc AST. Since `var T` is a subtype of `T`, a variable can also
/// be a constant.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum VariableArgument<T> {
    Identifier(Rc<str>),
    Constant(T),
}

/// Parse an [`ast::Constraint`] into a specific constraint type.
pub trait FlatZincConstraint: Sized {
    fn from_ast(
        constraint: &ast::Constraint,
        arrays: &BTreeMap<Rc<str>, ast::Node<ast::Array>>,
    ) -> Result<Self, InstanceError>;
}

/// Parse an [`ast::Annotation`] into a specific annotation type.
///
/// The difference with [`FlatZincConstraint::from_ast`] is that annotations can be ignored.
/// [`FlatZincAnnotation::from_ast`] can successfully parse an annotation into nothing, signifying
/// the annotation is not of interest in the final [`crate::Instance`].
pub trait FlatZincAnnotation: Sized {
    fn from_ast(annotation: &ast::Annotation) -> Result<Option<Self>, InstanceError>;
}

/// A default implementation that ignores all annotations.
impl FlatZincAnnotation for () {
    fn from_ast(_: &ast::Annotation) -> Result<Option<Self>, InstanceError> {
        Ok(None)
    }
}

/// Extract a value from an [`ast::Literal`].
pub trait FromLiteral: Sized {
    /// The [`Token`] that is expected for this implementation. Used to create error messages.
    fn expected() -> Token;

    /// Extract `Self` from a literal AST node.
    fn from_literal(node: &ast::Node<ast::Literal>) -> Result<Self, InstanceError>;
}

impl FromLiteral for i64 {
    fn expected() -> Token {
        Token::IntLiteral
    }

    fn from_literal(node: &ast::Node<ast::Literal>) -> Result<Self, InstanceError> {
        match &node.node {
            ast::Literal::Int(value) => Ok(*value),
            literal => Err(InstanceError::UnexpectedToken {
                expected: Token::IntLiteral,
                actual: literal.into(),
                span: node.span,
            }),
        }
    }
}

impl<T: FromLiteral> FromLiteral for VariableArgument<T> {
    fn expected() -> Token {
        Token::Variable(Box::new(T::expected()))
    }

    fn from_literal(node: &ast::Node<ast::Literal>) -> Result<Self, InstanceError> {
        match &node.node {
            ast::Literal::Identifier(identifier) => {
                Ok(VariableArgument::Identifier(Rc::clone(identifier)))
            }
            literal => T::from_literal(node)
                .map(VariableArgument::Constant)
                .map_err(|_| InstanceError::UnexpectedToken {
                    expected: <Self as FromLiteral>::expected(),
                    actual: literal.into(),
                    span: node.span,
                }),
        }
    }
}

impl FromLiteral for Rc<str> {
    fn expected() -> Token {
        Token::Identifier
    }

    fn from_literal(argument: &ast::Node<ast::Literal>) -> Result<Self, InstanceError> {
        match &argument.node {
            ast::Literal::Identifier(ident) => Ok(Rc::clone(ident)),

            node => Err(InstanceError::UnexpectedToken {
                expected: Token::Identifier,
                actual: node.into(),
                span: argument.span,
            }),
        }
    }
}

impl FromLiteral for bool {
    fn expected() -> Token {
        Token::BoolLiteral
    }

    fn from_literal(argument: &ast::Node<ast::Literal>) -> Result<Self, InstanceError> {
        match &argument.node {
            ast::Literal::Bool(boolean) => Ok(*boolean),

            node => Err(InstanceError::UnexpectedToken {
                expected: Token::BoolLiteral,
                actual: node.into(),
                span: argument.span,
            }),
        }
    }
}

impl FromLiteral for RangeList<i64> {
    fn expected() -> Token {
        Token::IntSetLiteral
    }

    fn from_literal(argument: &ast::Node<ast::Literal>) -> Result<Self, InstanceError> {
        match &argument.node {
            ast::Literal::IntSet(set) => Ok(set.clone()),

            node => Err(InstanceError::UnexpectedToken {
                expected: Token::IntSetLiteral,
                actual: node.into(),
                span: argument.span,
            }),
        }
    }
}

/// Extract an argument from the [`ast::Argument`] node.
pub trait FromArgument: Sized {
    fn from_argument(
        argument: &ast::Node<ast::Argument>,
        arrays: &BTreeMap<Rc<str>, ast::Node<ast::Array>>,
    ) -> Result<Self, InstanceError>;
}

impl<T: FromLiteral> FromArgument for T {
    fn from_argument(
        argument: &ast::Node<ast::Argument>,
        _: &BTreeMap<Rc<str>, ast::Node<ast::Array>>,
    ) -> Result<Self, InstanceError> {
        match &argument.node {
            ast::Argument::Literal(literal) => T::from_literal(literal),
            ast::Argument::Array(_) => Err(InstanceError::UnexpectedToken {
                expected: T::expected(),
                actual: Token::Array,
                span: argument.span,
            }),
        }
    }
}

impl<T: FromLiteral> FromArgument for Vec<T> {
    fn from_argument(
        argument: &ast::Node<ast::Argument>,
        arrays: &BTreeMap<Rc<str>, ast::Node<ast::Array>>,
    ) -> Result<Self, InstanceError> {
        let literals = match &argument.node {
            ast::Argument::Array(literals) => literals,

            ast::Argument::Literal(literal) => match &literal.node {
                ast::Literal::Identifier(identifier) => {
                    let array = arrays
                        .get(identifier)
                        .ok_or_else(|| InstanceError::UndefinedArray(identifier.as_ref().into()))?;

                    &array.node.contents
                }

                literal => {
                    return Err(InstanceError::UnexpectedToken {
                        expected: Token::Array,
                        actual: literal.into(),
                        span: argument.span,
                    })
                }
            },
        };

        literals
            .iter()
            .map(|literal| T::from_literal(literal))
            .collect::<Result<_, _>>()
    }
}

pub trait FromAnnotationArgument<Out = Self>: Sized {
    fn from_argument(argument: &ast::Node<ast::AnnotationArgument>) -> Result<Out, InstanceError>;
}

pub trait FromAnnotationLiteral: Sized {
    fn expected() -> Token;

    fn from_literal(literal: &ast::Node<ast::AnnotationLiteral>) -> Result<Self, InstanceError>;
}

impl<T: FromLiteral> FromAnnotationLiteral for T {
    fn expected() -> Token {
        T::expected()
    }

    fn from_literal(literal: &ast::Node<ast::AnnotationLiteral>) -> Result<Self, InstanceError> {
        match &literal.node {
            ast::AnnotationLiteral::BaseLiteral(base_literal) => T::from_literal(&ast::Node {
                node: base_literal.clone(),
                span: literal.span,
            }),
            ast::AnnotationLiteral::Annotation(_) => Err(InstanceError::UnexpectedToken {
                expected: T::expected(),
                actual: Token::AnnotationCall,
                span: literal.span,
            }),
        }
    }
}

impl<T: FromAnnotationLiteral> FromAnnotationArgument for T {
    fn from_argument(argument: &ast::Node<ast::AnnotationArgument>) -> Result<Self, InstanceError> {
        match &argument.node {
            ast::AnnotationArgument::Literal(literal) => T::from_literal(literal),

            node => Err(InstanceError::UnexpectedToken {
                expected: T::expected(),
                actual: node.into(),
                span: argument.span,
            }),
        }
    }
}

/// Parse a nested annotation from an annotation argument.
pub fn from_nested_annotation<T: FlatZincAnnotation>(
    argument: &ast::Node<ast::AnnotationArgument>,
) -> Result<T, InstanceError> {
    let annotation = match &argument.node {
        ast::AnnotationArgument::Literal(literal) => match &literal.node {
            ast::AnnotationLiteral::BaseLiteral(ast::Literal::Identifier(ident)) => {
                ast::Annotation::Atom(Rc::clone(ident))
            }
            ast::AnnotationLiteral::Annotation(annotation_call) => {
                ast::Annotation::Call(annotation_call.clone())
            }
            ast::AnnotationLiteral::BaseLiteral(lit) => {
                return Err(InstanceError::UnexpectedToken {
                    expected: Token::Annotation,
                    actual: lit.into(),
                    span: literal.span,
                });
            }
        },
        ast::AnnotationArgument::Array(_) => {
            return Err(InstanceError::UnexpectedToken {
                expected: Token::Annotation,
                actual: Token::Array,
                span: argument.span,
            });
        }
    };

    let outcome = T::from_ast(&annotation)?;

    outcome.ok_or_else(|| InstanceError::UnsupportedAnnotation(annotation.name().into()))
}
