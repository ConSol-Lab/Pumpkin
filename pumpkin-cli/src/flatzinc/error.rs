use std::num::TryFromIntError;

use thiserror::Error;

#[derive(Debug, Error)]
pub enum FlatZincError {
    #[error("failed to read instance file")]
    Io(#[from] std::io::Error),

    #[error("{0}")]
    SyntaxError(Box<str>),

    #[error("integer too big")]
    IntegerTooBig(#[from] TryFromIntError),

    #[error("constraint {constraint_id} expects {expected} arguments, got {actual}")]
    IncorrectNumberOfArguments {
        constraint_id: Box<str>,
        expected: usize,
        actual: usize,
    },

    #[error("unexpected expression")]
    UnexpectedExpr,

    #[error("the identifier '{identifier}' does not resolve to an '{expected_type}'")]
    InvalidIdentifier {
        identifier: Box<str>,
        expected_type: Box<str>,
    },
}
