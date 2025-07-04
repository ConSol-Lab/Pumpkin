use std::collections::BTreeMap;
use std::rc::Rc;

use crate::ast;
use crate::error::Token;
use crate::InstanceError;
use crate::IntVariable;

pub trait FromLiteral: Sized {
    fn expected() -> Token;

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
            ast::Literal::Identifier(_) => Err(InstanceError::UnexpectedToken {
                expected: Token::IntLiteral,
                actual: Token::Identifier,
                span: node.span,
            }),
            ast::Literal::Bool(_) => Err(InstanceError::UnexpectedToken {
                expected: Token::IntLiteral,
                actual: Token::BoolLiteral,
                span: node.span,
            }),
            ast::Literal::IntSet(_) => Err(InstanceError::UnexpectedToken {
                expected: Token::IntLiteral,
                actual: Token::IntSetLiteral,
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
            ast::Literal::Int(constant) => Ok(IntVariable::Constant(*constant)),
            ast::Literal::Bool(_) => Err(InstanceError::UnexpectedToken {
                expected: Token::IntVariable,
                actual: Token::BoolLiteral,
                span: node.span,
            }),
            ast::Literal::IntSet(_) => Err(InstanceError::UnexpectedToken {
                expected: Token::IntVariable,
                actual: Token::IntSetLiteral,
                span: node.span,
            }),
        }
    }
}

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

                ast::Literal::Int(_) => {
                    return Err(InstanceError::UnexpectedToken {
                        expected: Token::Array,
                        actual: Token::IntLiteral,
                        span: argument.span,
                    })
                }
                ast::Literal::Bool(_) => {
                    return Err(InstanceError::UnexpectedToken {
                        expected: Token::Array,
                        actual: Token::BoolLiteral,
                        span: argument.span,
                    })
                }
                ast::Literal::IntSet(_) => {
                    return Err(InstanceError::UnexpectedToken {
                        expected: Token::Array,
                        actual: Token::IntSetLiteral,
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
