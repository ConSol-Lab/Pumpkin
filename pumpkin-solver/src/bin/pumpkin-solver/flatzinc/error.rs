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

    #[error("the identifier '{identifier}' does not resolve to an '{expected_type}'")]
    InvalidIdentifier {
        identifier: Rc<str>,
        expected_type: Box<str>,
    },

    #[error("use of undefined array '{0}'")]
    UndefinedArray(Rc<str>),

    #[error("constraint '{0}' is not supported")]
    UnsupportedConstraint(String),

    /// Occurs when parsing a nested annotation.
    ///
    /// In this case, all possible arguments must be parsable into an annotation. If there is a
    /// value that cannot be parsed, this error variant is returned.
    #[error("annotation '{0}' is not supported")]
    UnsupportedAnnotation(String),

    #[error("expected {expected}, got {actual} at ({span_start}, {span_end})")]
    UnexpectedToken {
        expected: String,
        actual: String,
        span_start: usize,
        span_end: usize,
    },

    #[error("expected {expected} arguments, got {actual} at ({span_start}, {span_end})")]
    IncorrectNumberOfArguments {
        expected: usize,
        actual: usize,
        span_start: usize,
        span_end: usize,
    },

    #[error("value {0} does not fit in the required integer type")]
    IntegerOverflow(i64),
}

impl From<fzn_rs::fzn::FznError<'_>> for FlatZincError {
    fn from(value: fzn_rs::fzn::FznError<'_>) -> Self {
        match value {
            fzn_rs::fzn::FznError::LexError { reasons } => {
                // For now we only look at the first error. In the future, fzn-rs may produce
                // multiple errors.
                let reason = reasons[0].clone();

                let span = reason.span();
                let expected = reason
                    .expected()
                    .map(|pattern| format!("{pattern}, "))
                    .collect::<String>();

                FlatZincError::UnexpectedToken {
                    expected,
                    actual: reason
                        .found()
                        .map(|c| format!("{c}"))
                        .unwrap_or("".to_owned()),
                    span_start: span.start,
                    span_end: span.end,
                }
            }
            fzn_rs::fzn::FznError::ParseError { reasons } => {
                // For now we only look at the first error. In the future, fzn-rs may produce
                // multiple errors.
                let reason = reasons[0].clone();

                let span = reason.span();
                let expected = reason
                    .expected()
                    .map(|pattern| format!("{pattern}, "))
                    .collect::<String>();

                FlatZincError::UnexpectedToken {
                    expected,
                    actual: reason
                        .found()
                        .map(|token| format!("{token}"))
                        .unwrap_or("".to_owned()),
                    span_start: span.start,
                    span_end: span.end,
                }
            }
        }
    }
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
            fzn_rs::InstanceError::IncorrectNumberOfArguments {
                expected,
                actual,
                span,
            } => FlatZincError::IncorrectNumberOfArguments {
                expected,
                actual,
                span_start: span.start,
                span_end: span.end,
            },
            fzn_rs::InstanceError::IntegerOverflow(num) => FlatZincError::IntegerOverflow(num),
        }
    }
}
