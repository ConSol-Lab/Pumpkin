use std::rc::Rc;

use super::VariableExpr;
use crate::ast;
use crate::InstanceError;
use crate::Token;

/// Extract a value from an [`ast::Literal`].
pub trait FromLiteral: Sized {
    /// The [`Token`] that is expected for this implementation. Used to create error messages.
    fn expected() -> Token;

    /// Extract `Self` from a literal AST node.
    fn from_literal(node: &ast::Node<ast::Literal>) -> Result<Self, InstanceError>;
}

impl FromLiteral for i32 {
    fn expected() -> Token {
        Token::IntLiteral
    }

    fn from_literal(node: &ast::Node<ast::Literal>) -> Result<Self, InstanceError> {
        let integer = <i64 as FromLiteral>::from_literal(node)?;
        i32::try_from(integer).map_err(|_| InstanceError::IntegerOverflow(integer))
    }
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

impl<T: FromLiteral> FromLiteral for VariableExpr<T> {
    fn expected() -> Token {
        Token::Variable(Box::new(T::expected()))
    }

    fn from_literal(node: &ast::Node<ast::Literal>) -> Result<Self, InstanceError> {
        match &node.node {
            ast::Literal::Identifier(identifier) => {
                Ok(VariableExpr::Identifier(Rc::clone(identifier)))
            }
            literal => T::from_literal(node)
                .map(VariableExpr::Constant)
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

impl FromLiteral for ast::RangeList<i32> {
    fn expected() -> Token {
        Token::IntSetLiteral
    }

    fn from_literal(argument: &ast::Node<ast::Literal>) -> Result<Self, InstanceError> {
        let set = <ast::RangeList<i64> as FromLiteral>::from_literal(argument)?;

        set.into_iter()
            .map(|elem| i32::try_from(elem).map_err(|_| InstanceError::IntegerOverflow(elem)))
            .collect::<Result<_, _>>()
    }
}

impl FromLiteral for ast::RangeList<i64> {
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
