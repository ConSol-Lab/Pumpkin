//! This module contains traits that help with extracting a [`crate::Instance`] from an
//! [`crate::ast::Ast`].

use std::collections::BTreeMap;
use std::rc::Rc;

use crate::ast::RangeList;
use crate::ast::{self};
use crate::error::Token;
use crate::InstanceError;

/// Models a variable in the FlatZinc AST. Since `var T` is a subtype of `T`, a variable can also
/// be a constant.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum VariableExpr<T> {
    Identifier(Rc<str>),
    Constant(T),
}

/// Parse an [`ast::Constraint`] into a specific constraint type.
pub trait FlatZincConstraint: Sized {
    fn from_ast(
        constraint: &ast::Constraint,
        arrays: &BTreeMap<Rc<str>, ast::Node<ast::Array>>,
    ) -> Result<Self, InstanceError>;
}

/// Parse an [`ast::Annotation`] into a specific annotation type.
///
/// The difference with [`FlatZincConstraint::from_ast`] is that annotations can be ignored.
/// [`FlatZincAnnotation::from_ast`] can successfully parse an annotation into nothing, signifying
/// the annotation is not of interest in the final [`crate::Instance`].
pub trait FlatZincAnnotation: Sized {
    /// Parse a value of `Self` from the annotation node. Return `None` if the annotation node
    /// clearly is not relevant for `Self`, e.g. when the name is for a completely different
    /// annotation than `Self` models.
    fn from_ast(annotation: &ast::Annotation) -> Result<Option<Self>, InstanceError>;

    /// Parse an [`ast::Annotation`] into `Self` and produce an error if the annotation cannot be
    /// converted to a value of `Self`.
    fn from_ast_required(annotation: &ast::Annotation) -> Result<Self, InstanceError> {
        let outcome = Self::from_ast(annotation)?;

        // By default, failing to parse an annotation node into an annotation type is not
        // necessarily an error since the annotation node can be ignored. In this case, however,
        // we require a value to be present. Hence, if `outcome` is `None`, that is an error.
        outcome.ok_or_else(|| InstanceError::UnsupportedAnnotation(annotation.name().into()))
    }
}

/// A default implementation that ignores all annotations.
impl FlatZincAnnotation for () {
    fn from_ast(_: &ast::Annotation) -> Result<Option<Self>, InstanceError> {
        Ok(None)
    }
}

/// Extract a value from an [`ast::Literal`].
pub trait FromLiteral: Sized {
    /// The [`Token`] that is expected for this implementation. Used to create error messages.
    fn expected() -> Token;

    /// Extract `Self` from a literal AST node.
    fn from_literal(node: &ast::Node<ast::Literal>) -> Result<Self, InstanceError>;
}

impl FromLiteral for i32 {
    fn expected() -> Token {
        Token::IntLiteral
    }

    fn from_literal(node: &ast::Node<ast::Literal>) -> Result<Self, InstanceError> {
        let integer = <i64 as FromLiteral>::from_literal(node)?;
        i32::try_from(integer).map_err(|_| InstanceError::IntegerOverflow(integer))
    }
}

impl FromLiteral for i64 {
    fn expected() -> Token {
        Token::IntLiteral
    }

    fn from_literal(node: &ast::Node<ast::Literal>) -> Result<Self, InstanceError> {
        match &node.node {
            ast::Literal::Int(value) => Ok(*value),
            literal => Err(InstanceError::UnexpectedToken {
                expected: Token::IntLiteral,
                actual: literal.into(),
                span: node.span,
            }),
        }
    }
}

impl<T: FromLiteral> FromLiteral for VariableExpr<T> {
    fn expected() -> Token {
        Token::Variable(Box::new(T::expected()))
    }

    fn from_literal(node: &ast::Node<ast::Literal>) -> Result<Self, InstanceError> {
        match &node.node {
            ast::Literal::Identifier(identifier) => {
                Ok(VariableExpr::Identifier(Rc::clone(identifier)))
            }
            literal => T::from_literal(node)
                .map(VariableExpr::Constant)
                .map_err(|_| InstanceError::UnexpectedToken {
                    expected: <Self as FromLiteral>::expected(),
                    actual: literal.into(),
                    span: node.span,
                }),
        }
    }
}

impl FromLiteral for Rc<str> {
    fn expected() -> Token {
        Token::Identifier
    }

    fn from_literal(argument: &ast::Node<ast::Literal>) -> Result<Self, InstanceError> {
        match &argument.node {
            ast::Literal::Identifier(ident) => Ok(Rc::clone(ident)),

            node => Err(InstanceError::UnexpectedToken {
                expected: Token::Identifier,
                actual: node.into(),
                span: argument.span,
            }),
        }
    }
}

impl FromLiteral for bool {
    fn expected() -> Token {
        Token::BoolLiteral
    }

    fn from_literal(argument: &ast::Node<ast::Literal>) -> Result<Self, InstanceError> {
        match &argument.node {
            ast::Literal::Bool(boolean) => Ok(*boolean),

            node => Err(InstanceError::UnexpectedToken {
                expected: Token::BoolLiteral,
                actual: node.into(),
                span: argument.span,
            }),
        }
    }
}

impl FromLiteral for RangeList<i32> {
    fn expected() -> Token {
        Token::IntSetLiteral
    }

    fn from_literal(argument: &ast::Node<ast::Literal>) -> Result<Self, InstanceError> {
        let set = <RangeList<i64> as FromLiteral>::from_literal(argument)?;

        set.into_iter()
            .map(|elem| i32::try_from(elem).map_err(|_| InstanceError::IntegerOverflow(elem)))
            .collect::<Result<_, _>>()
    }
}

impl FromLiteral for RangeList<i64> {
    fn expected() -> Token {
        Token::IntSetLiteral
    }

    fn from_literal(argument: &ast::Node<ast::Literal>) -> Result<Self, InstanceError> {
        match &argument.node {
            ast::Literal::IntSet(set) => Ok(set.clone()),

            node => Err(InstanceError::UnexpectedToken {
                expected: Token::IntSetLiteral,
                actual: node.into(),
                span: argument.span,
            }),
        }
    }
}

/// Extract an argument from the [`ast::Argument`] node.
pub trait FromArgument: Sized {
    fn from_argument(
        argument: &ast::Node<ast::Argument>,
        arrays: &BTreeMap<Rc<str>, ast::Node<ast::Array>>,
    ) -> Result<Self, InstanceError>;
}

impl<T: FromLiteral> FromArgument for T {
    fn from_argument(
        argument: &ast::Node<ast::Argument>,
        _: &BTreeMap<Rc<str>, ast::Node<ast::Array>>,
    ) -> Result<Self, InstanceError> {
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

impl<T: FromLiteral> FromArgument for Vec<T> {
    fn from_argument(
        argument: &ast::Node<ast::Argument>,
        arrays: &BTreeMap<Rc<str>, ast::Node<ast::Array>>,
    ) -> Result<Self, InstanceError> {
        let literals = match &argument.node {
            ast::Argument::Array(literals) => literals,

            ast::Argument::Literal(literal) => match &literal.node {
                ast::Literal::Identifier(identifier) => {
                    let array = arrays
                        .get(identifier)
                        .ok_or_else(|| InstanceError::UndefinedArray(identifier.as_ref().into()))?;

                    &array.node.contents
                }

                literal => {
                    return Err(InstanceError::UnexpectedToken {
                        expected: Token::Array,
                        actual: literal.into(),
                        span: argument.span,
                    })
                }
            },
        };

        literals
            .iter()
            .map(|literal| T::from_literal(literal))
            .collect::<Result<_, _>>()
    }
}

/// Parse a value from an [`ast::AnnotationArgument`].
///
/// Any type that implements [`FromAnnotationLiteral`] also implements [`FromAnnotationArgument`].
pub trait FromAnnotationArgument: Sized {
    fn from_argument(argument: &ast::Node<ast::AnnotationArgument>) -> Result<Self, InstanceError>;
}

/// Parse a value from an [`ast::AnnotationLiteral`].
pub trait FromAnnotationLiteral: Sized {
    fn expected() -> Token;

    fn from_literal(literal: &ast::Node<ast::AnnotationLiteral>) -> Result<Self, InstanceError>;
}

impl<T: FromLiteral> FromAnnotationLiteral for T {
    fn expected() -> Token {
        T::expected()
    }

    fn from_literal(literal: &ast::Node<ast::AnnotationLiteral>) -> Result<Self, InstanceError> {
        match &literal.node {
            ast::AnnotationLiteral::BaseLiteral(base_literal) => T::from_literal(&ast::Node {
                node: base_literal.clone(),
                span: literal.span,
            }),
            ast::AnnotationLiteral::Annotation(_) => Err(InstanceError::UnexpectedToken {
                expected: T::expected(),
                actual: Token::AnnotationCall,
                span: literal.span,
            }),
        }
    }
}

impl<T: FromAnnotationLiteral> FromAnnotationArgument for T {
    fn from_argument(argument: &ast::Node<ast::AnnotationArgument>) -> Result<Self, InstanceError> {
        match &argument.node {
            ast::AnnotationArgument::Literal(literal) => T::from_literal(literal),

            node => Err(InstanceError::UnexpectedToken {
                expected: T::expected(),
                actual: node.into(),
                span: argument.span,
            }),
        }
    }
}

impl<T: FromAnnotationLiteral> FromAnnotationArgument for Vec<T> {
    fn from_argument(argument: &ast::Node<ast::AnnotationArgument>) -> Result<Self, InstanceError> {
        match &argument.node {
            ast::AnnotationArgument::Array(array) => array
                .iter()
                .map(|literal| T::from_literal(literal))
                .collect(),

            node => Err(InstanceError::UnexpectedToken {
                expected: Token::Array,
                actual: node.into(),
                span: argument.span,
            }),
        }
    }
}

/// Parse an [`ast::AnnotationArgument`] as an annotation. This needs to be a separate trait from
/// [`FromAnnotationArgument`] so it does not collide wiith implementations for literals.
pub trait FromNestedAnnotation: Sized {
    fn from_argument(argument: &ast::Node<ast::AnnotationArgument>) -> Result<Self, InstanceError>;
}

/// Converts an [`ast::AnnotationLiteral`] to an [`ast::Annotation`], or produces an error if that
/// is not possible.
fn annotation_literal_to_annotation(
    literal: &ast::Node<ast::AnnotationLiteral>,
) -> Result<ast::Annotation, InstanceError> {
    match &literal.node {
        ast::AnnotationLiteral::BaseLiteral(ast::Literal::Identifier(ident)) => {
            Ok(ast::Annotation::Atom(Rc::clone(ident)))
        }
        ast::AnnotationLiteral::Annotation(annotation_call) => {
            Ok(ast::Annotation::Call(annotation_call.clone()))
        }
        ast::AnnotationLiteral::BaseLiteral(lit) => Err(InstanceError::UnexpectedToken {
            expected: Token::Annotation,
            actual: lit.into(),
            span: literal.span,
        }),
    }
}

impl<Ann: FlatZincAnnotation> FromNestedAnnotation for Ann {
    fn from_argument(argument: &ast::Node<ast::AnnotationArgument>) -> Result<Ann, InstanceError> {
        let annotation = match &argument.node {
            ast::AnnotationArgument::Literal(literal) => annotation_literal_to_annotation(literal)?,
            ast::AnnotationArgument::Array(_) => {
                return Err(InstanceError::UnexpectedToken {
                    expected: Token::Annotation,
                    actual: Token::Array,
                    span: argument.span,
                });
            }
        };

        Ann::from_ast_required(&annotation)
    }
}

impl<Ann: FlatZincAnnotation> FromNestedAnnotation for Vec<Ann> {
    fn from_argument(argument: &ast::Node<ast::AnnotationArgument>) -> Result<Self, InstanceError> {
        match &argument.node {
            ast::AnnotationArgument::Array(elements) => elements
                .iter()
                .map(|literal| {
                    let annotation = annotation_literal_to_annotation(literal)?;
                    Ann::from_ast_required(&annotation)
                })
                .collect::<Result<_, _>>(),
            ast::AnnotationArgument::Literal(lit) => Err(InstanceError::UnexpectedToken {
                expected: Token::Array,
                actual: (&lit.node).into(),
                span: argument.span,
            }),
        }
    }
}
