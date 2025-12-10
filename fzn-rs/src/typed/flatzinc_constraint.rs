use super::FromLiteral;
use crate::InstanceError;
use crate::Token;
use crate::ast;

/// Parse a constraint from the given [`ast::Constraint`].
pub trait FlatZincConstraint: Sized {
    fn from_ast(constraint: &ast::Node<ast::Constraint>) -> Result<Self, InstanceError>;
}

/// Extract an argument from the [`ast::Argument`] node.
pub trait FromArgument: Sized {
    fn from_argument(argument: &ast::Node<ast::Argument>) -> Result<Self, InstanceError>;
}

impl<T: FromLiteral> FromArgument for T {
    fn from_argument(argument: &ast::Node<ast::Argument>) -> Result<Self, InstanceError> {
        match &argument.node {
            ast::Argument::Literal(literal) => T::from_literal(literal),
            ast::Argument::Array(_) => Err(InstanceError::UnexpectedToken {
                expected: T::expected(),
                actual: Token::Array,
                span: argument.span,
            }),
        }
    }
}
