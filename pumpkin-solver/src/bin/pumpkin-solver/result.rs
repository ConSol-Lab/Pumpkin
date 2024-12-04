use std::fmt::Display;

use thiserror::Error;

use crate::flatzinc::error::FlatZincError;
use crate::parsers::dimacs::DimacsParseError;

pub(crate) type PumpkinResult<T> = Result<T, PumpkinError>;

#[derive(Error, Debug)]

pub(crate) enum PumpkinError {
    #[error("IO error, more details: {0}")]
    IOError(#[from] std::io::Error),
    #[error("The file {0} is not supported.")]
    InvalidInstanceFile(String),
    #[error("The dimacs file was invalid, more details: {0}")]
    InvalidDimacs(#[from] DimacsParseError),
    #[error("Failed to run flatzinc model, more details: {0}")]
    FlatZinc(#[from] FlatZincError),
    #[error("Proof generation for {0} is not supported.")]
    ProofGenerationNotSupported(String),
}

impl PumpkinError {
    pub(crate) fn invalid_instance(path: impl Display) -> Self {
        Self::InvalidInstanceFile(format!("{}", path))
    }
}
