use std::num::TryFromIntError;

use thiserror::Error;

#[derive(Debug, Error)]
pub enum FlatZincError {
    #[error("failed to read instance file: {0}")]
    Io(#[from] std::io::Error),

    #[error("{0}")]
    SyntaxError(Box<str>),

    #[error("{0} variables are not supported")]
    UnsupportedVariable(Box<str>),

    #[error("integer too big")]
    IntegerTooBig(#[from] TryFromIntError),

    #[error("constraint {constraint_id} expects {expected} arguments, got {actual}")]
    IncorrectNumberOfArguments {
        constraint_id: Box<str>,
        expected: usize,
        actual: usize,
    },

    #[error("unexpected expression")]
    UnexpectedExpr,

    #[error("the identifier '{identifier}' does not resolve to an '{expected_type}'")]
    InvalidIdentifier {
        identifier: Box<str>,
        expected_type: Box<str>,
    },

    #[error(
        "the declaration of '{0}' makes it equal to another variable, which the proof cannot \
         state"
    )]
    VariableAlias(Box<str>),

    #[error("missing solve item")]
    MissingSolveItem,

    #[error(
        "a croc proof cannot be logged when enumerating all solutions of a satisfaction problem: \
         the clauses that exclude each solution found are not derivable"
    )]
    ProofOfSolutionEnumeration,
}
