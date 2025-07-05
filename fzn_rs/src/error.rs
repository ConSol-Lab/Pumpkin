use std::fmt::Display;

use crate::ast;

#[derive(Clone, Debug, thiserror::Error)]
pub enum InstanceError {
    #[error("constraint '{0}' is not supported")]
    UnsupportedConstraint(String),

    #[error("expected {expected}, got {actual} at {span}")]
    UnexpectedToken {
        expected: Token,
        actual: Token,
        span: ast::Span,
    },

    #[error("array {0} is undefined")]
    UndefinedArray(String),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Token {
    Identifier,
    IntLiteral,
    BoolLiteral,
    IntSetLiteral,

    Array,
    Variable(Box<Token>),
}

impl From<&'_ ast::Literal> for Token {
    fn from(value: &'_ ast::Literal) -> Self {
        match value {
            ast::Literal::Int(_) => Token::IntLiteral,
            ast::Literal::Identifier(_) => Token::Identifier,
            ast::Literal::Bool(_) => Token::BoolLiteral,
            ast::Literal::IntSet(_) => Token::IntSetLiteral,
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Identifier => write!(f, "identifier"),

            Token::IntLiteral => write!(f, "int"),
            Token::BoolLiteral => write!(f, "bool"),
            Token::IntSetLiteral => write!(f, "int set"),

            Token::Array => write!(f, "array"),
            Token::Variable(token) => write!(f, "{token} variable"),
        }
    }
}
