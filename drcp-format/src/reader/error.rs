use thiserror::Error;

#[derive(Debug, Error)]
pub enum DrcpError {
    #[error("Failed to read: {0}")]
    Io(#[from] std::io::Error),

    #[error("Syntax error: {0}")]
    Syntax(String),
}

impl From<nom::Err<nom::error::Error<&str>>> for DrcpError {
    fn from(value: nom::Err<nom::error::Error<&str>>) -> Self {
        DrcpError::Syntax(value.to_string())
    }
}
