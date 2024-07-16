use std::fmt::Display;

use thiserror::Error;

use crate::flatzinc::error::FlatZincError;

pub(crate) type PumpkinResult<T> = Result<T, PumpkinError>;

#[derive(Error, Debug)]
#[allow(dead_code)]
pub(crate) enum PumpkinError {
    #[error("Hard clauses violated")]
    InconsistentSolution,
    #[error("Reported objective value is lower than the actual value")]
    InconsistentObjective,
    #[error("IO error, more details: {0}")]
    IOError(#[from] std::io::Error),
    #[error("Failed to read file {1}, more details: {0}")]
    FileReadingError(std::io::Error, String),
    #[error("The file {0} is not supported.")]
    InvalidInstanceFile(String),
    #[error("No file location given")]
    MissingFileError,
    // todo: add back parsing error
    // #[error("The dimacs file was invalid, more details: {0}")]
    // InvalidDimacs(#[from] DimacsParseError),
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
