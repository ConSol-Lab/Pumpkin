use std::{collections::BTreeMap, marker::PhantomData, rc::Rc};

use crate::{ast, InstanceError, Token};

use super::{FromArgument, FromLiteral, VariableExpr};

/// Models an array in a constraint argument.
///
/// ## Example
/// ```
/// use fzn_rs::ArrayExpr;
/// use fzn_rs::FlatZincConstraint;
/// use fzn_rs::VariableExpr;
///
/// #[derive(FlatZincConstraint)]
/// struct Linear {
///     /// An array of constants.
///     weights: ArrayExpr<i64>,
///     /// An array of variables.
///     variables: ArrayExpr<VariableExpr<i64>>,
/// }
/// ```
///
/// Use [`crate::TypedInstance::resolve_array`] to access the elements in the array.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ArrayExpr<T> {
    expr: ArrayExprImpl,
    ty: PhantomData<T>,
}

impl<T> ArrayExpr<T>
where
    T: FromLiteral,
{
    pub(crate) fn resolve<'a, Ann>(
        &'a self,
        arrays: &'a BTreeMap<Rc<str>, ast::Array<Ann>>,
    ) -> Option<impl ExactSizeIterator<Item = Result<T, InstanceError>> + 'a> {
        match &self.expr {
            ArrayExprImpl::Identifier(ident) => arrays.get(ident).map(|array| {
                GenericIterator(Box::new(
                    array.contents.iter().map(<T as FromLiteral>::from_literal),
                ))
            }),
            ArrayExprImpl::Array(array) => Some(GenericIterator(Box::new(
                array.contents.iter().map(<T as FromLiteral>::from_literal),
            ))),
        }
    }
}

impl<T> FromArgument for ArrayExpr<T> {
    fn from_argument(argument: &ast::Node<ast::Argument>) -> Result<Self, InstanceError> {
        match &argument.node {
            ast::Argument::Array(contents) => Ok(ArrayExpr {
                expr: ArrayExprImpl::Array(ast::Array {
                    contents: contents.clone(),
                    annotations: vec![],
                }),
                ty: PhantomData,
            }),
            ast::Argument::Literal(ast::Node {
                node: ast::Literal::Identifier(ident),
                ..
            }) => Ok(ArrayExpr {
                expr: ArrayExprImpl::Identifier(Rc::clone(ident)),
                ty: PhantomData,
            }),
            ast::Argument::Literal(literal) => Err(InstanceError::UnexpectedToken {
                expected: Token::Array,
                actual: Token::from(&literal.node),
                span: literal.span,
            }),
        }
    }
}

impl<T> From<Vec<VariableExpr<T>>> for ArrayExpr<T>
where
    ast::Literal: From<T>,
{
    fn from(value: Vec<VariableExpr<T>>) -> Self {
        ArrayExpr {
            expr: ArrayExprImpl::Array(ast::Array {
                contents: value
                    .into_iter()
                    .map(|value| ast::Node {
                        node: match value {
                            VariableExpr::Identifier(ident) => ast::Literal::Identifier(ident),
                            VariableExpr::Constant(value) => ast::Literal::from(value),
                        },
                        span: ast::Span {
                            start: usize::MAX,
                            end: usize::MAX,
                        },
                    })
                    .collect(),
                annotations: vec![],
            }),
            ty: PhantomData,
        }
    }
}

impl<T> From<Vec<T>> for ArrayExpr<T>
where
    ast::Literal: From<T>,
{
    fn from(value: Vec<T>) -> Self {
        ArrayExpr {
            expr: ArrayExprImpl::Array(ast::Array {
                contents: value
                    .into_iter()
                    .map(|value| ast::Node {
                        node: ast::Literal::from(value),
                        span: ast::Span {
                            start: usize::MAX,
                            end: usize::MAX,
                        },
                    })
                    .collect(),
                annotations: vec![],
            }),
            ty: PhantomData,
        }
    }
}

/// The actual array expression, which is either an identifier or the array.
///
/// This is a private type as all access to the array should go through [`ArrayExpr::resolve`].
#[derive(Clone, Debug, PartialEq, Eq)]
enum ArrayExprImpl {
    Identifier(Rc<str>),
    Array(ast::Array<()>),
}

/// A boxed dyn [`ExactSizeIterator`] which is returned from [`ArrayExpr::resolve`].
struct GenericIterator<'a, T>(Box<dyn ExactSizeIterator<Item = T> + 'a>);

impl<T> Iterator for GenericIterator<'_, T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }
}

impl<T> ExactSizeIterator for GenericIterator<'_, T> {}
