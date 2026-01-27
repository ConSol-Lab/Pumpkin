use std::fmt::Display;

use thiserror::Error;

use crate::flatzinc::error::FlatZincError;

pub(crate) type PumpkinResult<T> = Result<T, PumpkinError>;

#[derive(Error, Debug)]

pub(crate) enum PumpkinError {
    #[error("IO error, more details: {0}")]
    IOError(#[from] std::io::Error),
    #[error("The file {0} is not supported.")]
    InvalidInstanceFile(String),
    #[error("Failed to run flatzinc model, more details: {0}")]
    FlatZinc(#[from] FlatZincError),
}

impl PumpkinError {
    pub(crate) fn invalid_instance(path: impl Display) -> Self {
        Self::InvalidInstanceFile(format!("{path}"))
    }
}
