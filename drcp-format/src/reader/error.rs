use std::io;
use std::num::NonZero;

#[cfg(doc)]
use super::ProofReader;

/// The errors that can be encountered by the [`ProofReader`].
#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("failed to read from source: {0}")]
    IoError(#[from] io::Error),

    #[error("failed to parse proof line {line_nr} {span:?}: {reason}")]
    ParseError {
        line_nr: usize,
        reason: String,
        span: (usize, usize),
    },

    #[error("undefined atomic {code} on line {line}")]
    UndefinedAtomic { line: usize, code: NonZero<i32> },
}
