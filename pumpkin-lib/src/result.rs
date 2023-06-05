use thiserror::Error;

pub type PumpkinResult<T> = Result<T, PumpkinError>;

#[derive(Error, Debug)]
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
}
