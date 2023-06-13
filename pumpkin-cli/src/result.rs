use thiserror::Error;

use crate::parsers::dimacs::DimacsParseError;

pub type PumpkinResult<T> = Result<T, PumpkinError>;

#[derive(Error, Debug)]
#[allow(dead_code)]
pub enum PumpkinError {
    #[error("Hard clauses violated")]
    InconsistentSolution,
    #[error("Reported objective value is lower than the actual value")]
    InconsistentObjective,
    #[error("IO error, more details: {0}")]
    IOError(#[from] std::io::Error),
    #[error("Failed to read file {1}, more details: {0}")]
    FileReadingError(std::io::Error, String),
    #[error("The supplied path is not supported.")]
    InvalidInstanceFile,
    #[error("No file location given")]
    MissingFileError,
    #[error("The dimacs file was invalid")]
    InvalidDimacs(#[from] DimacsParseError),
}
