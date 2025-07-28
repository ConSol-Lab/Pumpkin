use std::rc::Rc;

use super::FromLiteral;
use crate::ast;
use crate::InstanceError;
use crate::Token;

/// Parse an [`ast::Annotation`] into a specific annotation type.
///
/// The difference with [`crate::FlatZincConstraint::from_ast`] is that annotations can be ignored.
/// [`FlatZincAnnotation::from_ast`] can successfully parse an annotation into nothing, signifying
/// the annotation is not of interest in the final [`crate::TypedInstance`].
pub trait FlatZincAnnotation: Sized {
    /// Parse a value of `Self` from the annotation node. Return `None` if the annotation node
    /// clearly is not relevant for `Self`, e.g. when the name is for a completely different
    /// annotation than `Self` models.
    fn from_ast(annotation: &ast::Node<ast::Annotation>) -> Result<Option<Self>, InstanceError>;

    /// Parse an [`ast::Annotation`] into `Self` and produce an error if the annotation cannot be
    /// converted to a value of `Self`.
    fn from_ast_required(annotation: &ast::Node<ast::Annotation>) -> Result<Self, InstanceError> {
        let outcome = Self::from_ast(annotation)?;

        // By default, failing to parse an annotation node into an annotation type is not
        // necessarily an error since the annotation node can be ignored. In this case, however,
        // we require a value to be present. Hence, if `outcome` is `None`, that is an error.
        outcome.ok_or_else(|| InstanceError::UnsupportedAnnotation(annotation.node.name().into()))
    }
}

/// A default implementation that ignores all annotations.
impl FlatZincAnnotation for () {
    fn from_ast(_: &ast::Node<ast::Annotation>) -> Result<Option<Self>, InstanceError> {
        Ok(None)
    }
}

/// Parse a value from an [`ast::AnnotationArgument`].
///
/// Any type that implements [`FromAnnotationLiteral`] also implements [`FromAnnotationArgument`].
pub trait FromAnnotationArgument: Sized {
    fn from_argument(argument: &ast::Node<ast::AnnotationArgument>) -> Result<Self, InstanceError>;
}

/// Parse a value from an [`ast::AnnotationLiteral`].
pub trait FromAnnotationLiteral: FromLiteral + Sized {
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
            ast::AnnotationArgument::Literal(literal) => {
                <T as FromAnnotationLiteral>::from_literal(literal)
            }

            node => Err(InstanceError::UnexpectedToken {
                expected: <T as FromAnnotationLiteral>::expected(),
                actual: node.into(),
                span: argument.span,
            }),
        }
    }
}

// impl<T: FromAnnotationLiteral> FromAnnotationArgument for ArrayExpr<T> {
//     fn from_argument(argument: &ast::Node<ast::AnnotationArgument>) -> Result<Self, InstanceError> {
//         match &argument.node {
//             ast::AnnotationArgument::Array(array) => {
//                 let contents =
//                 array
//                 .iter()
//                 .map(|literal| T::from_literal(literal))
//                 .collect();
//                 ArrayExpr,
//
//             node => Err(InstanceError::UnexpectedToken {
//                 expected: Token::Array,
//                 actual: node.into(),
//                 span: argument.span,
//             }),
//         }
//     }
// }

/// Parse an [`ast::AnnotationArgument`] as an annotation. This needs to be a separate trait from
/// [`FromAnnotationArgument`] so it does not collide wiith implementations for literals.
pub trait FromNestedAnnotation: Sized {
    fn from_argument(argument: &ast::Node<ast::AnnotationArgument>) -> Result<Self, InstanceError>;
}

/// Converts an [`ast::AnnotationLiteral`] to an [`ast::Annotation`], or produces an error if that
/// is not possible.
fn annotation_literal_to_annotation(
    literal: &ast::Node<ast::AnnotationLiteral>,
) -> Result<ast::Node<ast::Annotation>, InstanceError> {
    match &literal.node {
        ast::AnnotationLiteral::BaseLiteral(ast::Literal::Identifier(ident)) => Ok(ast::Node {
            node: ast::Annotation::Atom(Rc::clone(ident)),
            span: literal.span,
        }),
        ast::AnnotationLiteral::Annotation(annotation_call) => Ok(ast::Node {
            node: ast::Annotation::Call(annotation_call.clone()),
            span: literal.span,
        }),
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
