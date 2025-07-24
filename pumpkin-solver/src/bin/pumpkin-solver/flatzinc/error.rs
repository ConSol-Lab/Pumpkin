use std::{num::TryFromIntError, rc::Rc};

use thiserror::Error;

#[derive(Debug, Error)]
pub(crate) enum FlatZincError {
    #[error("failed to read instance file: {0}")]
    Io(#[from] std::io::Error),

    #[error("{0} variables are not supported")]
    UnsupportedVariable(Box<str>),

    #[error("integer too big")]
    IntegerTooBig(#[from] TryFromIntError),

    #[error("unexpected expression")]
    UnexpectedExpr,

    #[error("the identifier '{identifier}' does not resolve to an '{expected_type}'")]
    InvalidIdentifier {
        identifier: Rc<str>,
        expected_type: Box<str>,
    },
}
