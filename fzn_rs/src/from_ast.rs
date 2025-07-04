//! This module contains traits that help with extracting a [`crate::Instance`] from an
//! [`crate::ast::Ast`].

use std::collections::BTreeMap;
use std::rc::Rc;

use crate::ast;
use crate::error::Token;
use crate::InstanceError;
use crate::IntVariable;

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
    fn from_literal(
        node: &ast::Node<ast::Literal>,
        arrays: &BTreeMap<Rc<str>, ast::Node<ast::Array>>,
    ) -> Result<Self, InstanceError>;
}

impl FromLiteral for i64 {
    fn expected() -> Token {
        Token::IntLiteral
    }

    fn from_literal(
        node: &ast::Node<ast::Literal>,
        _: &BTreeMap<Rc<str>, ast::Node<ast::Array>>,
    ) -> Result<Self, InstanceError> {
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

impl FromLiteral for IntVariable {
    fn expected() -> Token {
        Token::IntVariable
    }

    fn from_literal(
        node: &ast::Node<ast::Literal>,
        _: &BTreeMap<Rc<str>, ast::Node<ast::Array>>,
    ) -> Result<Self, InstanceError> {
        match &node.node {
            ast::Literal::Identifier(identifier) => {
                Ok(IntVariable::Identifier(Rc::clone(identifier)))
            }
            literal => Err(InstanceError::UnexpectedToken {
                expected: Token::IntVariable,
                actual: literal.into(),
                span: node.span,
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
        arrays: &BTreeMap<Rc<str>, ast::Node<ast::Array>>,
    ) -> Result<Self, InstanceError> {
        match &argument.node {
            ast::Argument::Literal(literal) => T::from_literal(literal, arrays),
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
            .map(|literal| T::from_literal(literal, arrays))
            .collect::<Result<_, _>>()
    }
}
