use std::fmt::Display;

use crate::ast;

#[derive(Clone, Debug, thiserror::Error)]
pub enum InstanceError {
    #[error("constraint '{0}' is not supported")]
    UnsupportedConstraint(String),

    #[error("expected {expected}, got {actual} at {span}")]
    UnexpectedToken {
        expected: Token,
        actual: Token,
        span: ast::Span,
    },

    #[error("array {0} is undefined")]
    UndefinedArray(String),

    #[error("expected {expected} arguments, got {actual}")]
    IncorrectNumberOfArguments { expected: usize, actual: usize },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Token {
    Identifier,
    IntLiteral,
    BoolLiteral,
    IntSetLiteral,

    Array,
    Variable(Box<Token>),
    AnnotationCall,
}

impl From<&'_ ast::Literal> for Token {
    fn from(value: &'_ ast::Literal) -> Self {
        match value {
            ast::Literal::Int(_) => Token::IntLiteral,
            ast::Literal::Identifier(_) => Token::Identifier,
            ast::Literal::Bool(_) => Token::BoolLiteral,
            ast::Literal::IntSet(_) => Token::IntSetLiteral,
        }
    }
}

impl From<&'_ ast::AnnotationArgument> for Token {
    fn from(value: &'_ ast::AnnotationArgument) -> Self {
        match value {
            ast::AnnotationArgument::Array(_) => Token::Array,
            ast::AnnotationArgument::Literal(literal) => (&literal.node).into(),
        }
    }
}

impl From<&'_ ast::AnnotationLiteral> for Token {
    fn from(value: &'_ ast::AnnotationLiteral) -> Self {
        match value {
            ast::AnnotationLiteral::BaseLiteral(literal) => literal.into(),
            ast::AnnotationLiteral::Annotation(_) => Token::AnnotationCall,
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Identifier => write!(f, "identifier"),

            Token::IntLiteral => write!(f, "int"),
            Token::BoolLiteral => write!(f, "bool"),
            Token::IntSetLiteral => write!(f, "int set"),

            Token::AnnotationCall => write!(f, "annotation"),

            Token::Array => write!(f, "array"),
            Token::Variable(token) => write!(f, "{token} variable"),
        }
    }
}
