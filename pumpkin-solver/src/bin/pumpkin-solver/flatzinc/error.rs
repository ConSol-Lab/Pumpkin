use std::num::TryFromIntError;
use std::rc::Rc;

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

    #[error("use of undefined array '{0}'")]
    UndefinedArray(Rc<str>),

    #[error("constraint '{0}' is not supported")]
    UnsupportedConstraint(String),

    #[error("annotation '{0}' is not supported")]
    UnsupportedAnnotation(String),

    #[error("expected {expected}, got {actual} at ({span_start}, {span_end})")]
    UnexpectedToken {
        expected: String,
        actual: String,
        span_start: usize,
        span_end: usize,
    },

    #[error("expected {expected} arguments, got {actual}")]
    IncorrectNumberOfArguments { expected: usize, actual: usize },

    #[error("value {0} does not fit in the required integer type")]
    IntegerOverflow(i64),
}

impl From<fzn_rs::InstanceError> for FlatZincError {
    fn from(value: fzn_rs::InstanceError) -> Self {
        match value {
            fzn_rs::InstanceError::UnsupportedConstraint(c) => {
                FlatZincError::UnsupportedConstraint(c)
            }
            fzn_rs::InstanceError::UnsupportedAnnotation(a) => {
                FlatZincError::UnsupportedAnnotation(a)
            }
            fzn_rs::InstanceError::UnexpectedToken {
                expected,
                actual,
                span,
            } => FlatZincError::UnexpectedToken {
                expected: format!("{expected}"),
                actual: format!("{actual}"),
                span_start: span.start,
                span_end: span.end,
            },
            fzn_rs::InstanceError::UndefinedArray(a) => FlatZincError::UndefinedArray(a),
            fzn_rs::InstanceError::IncorrectNumberOfArguments { expected, actual } => {
                FlatZincError::IncorrectNumberOfArguments { expected, actual }
            }
            fzn_rs::InstanceError::IntegerOverflow(num) => FlatZincError::IntegerOverflow(num),
        }
    }
}
