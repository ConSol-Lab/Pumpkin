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
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Token {
    Identifier,
    IntLiteral,
    BoolLiteral,
    IntSetLiteral,

    IntVariable,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Identifier => write!(f, "identifier"),

            Token::IntLiteral => write!(f, "int literal"),
            Token::BoolLiteral => write!(f, "bool literal"),
            Token::IntSetLiteral => write!(f, "int set literal"),

            Token::IntVariable => write!(f, "int variable"),
        }
    }
}
