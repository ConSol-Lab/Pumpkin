use std::collections::BTreeMap;
use std::rc::Rc;

use super::ArrayExpr;
use super::FlatZincAnnotation;
use super::FlatZincConstraint;
use super::FromAnnotationLiteral;
use super::FromLiteral;
use super::VariableExpr;
use crate::ast;
use crate::AnnotatedConstraint;
use crate::InstanceError;

/// A fully typed representation of a FlatZinc instance.
///
/// It is generic over the type of constraints, as well as the annotations for variables, arrays,
/// constraints, and solve.
#[derive(Clone, Debug)]
pub struct TypedInstance<
    Int,
    Constraint,
    VariableAnnotations = (),
    ArrayAnnotations = (),
    ConstraintAnnotations = (),
    SolveAnnotations = (),
> {
    /// The variables that are in the instance.
    ///
    /// The key is the identifier of the variable, and the value is the domain of the variable.
    pub variables: BTreeMap<Rc<str>, ast::Variable<VariableAnnotations>>,

    /// The arrays in the instance.
    ///
    /// The key is the identifier of the array, and the value is the array itself.
    pub arrays: BTreeMap<Rc<str>, ast::Array<ArrayAnnotations>>,

    /// The constraints in the instance.
    pub constraints: Vec<AnnotatedConstraint<Constraint, ConstraintAnnotations>>,

    /// The solve item indicating how to solve the model.
    pub solve: Solve<Int, SolveAnnotations>,
}

/// Specifies how to solve a [`TypedInstance`].
///
/// This is generic over the integer type.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Solve<Int, Ann> {
    pub method: ast::Node<Method<Int>>,
    pub annotations: Vec<ast::Node<Ann>>,
}

/// Indicate whether the model is an optimisation or satisfaction model.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Method<Int> {
    Satisfy,
    Optimize {
        direction: ast::OptimizationDirection,
        objective: VariableExpr<Int>,
    },
}

#[derive(Clone, Debug, thiserror::Error)]
#[error("array '{0}' is undefined")]
pub struct UndefinedArrayError(String);

impl<Int, TConstraint, VAnnotations, AAnnotations, CAnnotations, SAnnotations>
    TypedInstance<Int, TConstraint, VAnnotations, AAnnotations, CAnnotations, SAnnotations>
{
    /// Get the elements in an [`ArrayExpr`].
    pub fn resolve_array<'a, T>(
        &'a self,
        array_expr: &'a ArrayExpr<T>,
    ) -> Result<impl ExactSizeIterator<Item = Result<T, InstanceError>> + 'a, UndefinedArrayError>
    where
        T: FromAnnotationLiteral,
    {
        array_expr
            .resolve(&self.arrays)
            .map_err(|identifier| UndefinedArrayError(identifier.as_ref().into()))
    }
}

impl<Int, TConstraint, VAnnotations, AAnotations, CAnnotations, SAnnotations>
    TypedInstance<Int, TConstraint, VAnnotations, AAnotations, CAnnotations, SAnnotations>
where
    TConstraint: FlatZincConstraint,
    VAnnotations: FlatZincAnnotation,
    AAnotations: FlatZincAnnotation,
    CAnnotations: FlatZincAnnotation,
    SAnnotations: FlatZincAnnotation,
    VariableExpr<Int>: FromLiteral,
{
    /// Create a [`TypedInstance`] from an [`ast::Ast`].
    ///
    /// This parses the constraints and annotations, and can fail e.g. if the number or type of
    /// arguments do not match what is expected in the parser.
    ///
    /// This does _not_ type-check the variables. I.e., if a constraint takes a `var int`, but
    /// is provided with an identifier of a `var bool`, then this function will gladly accept that.
    pub fn from_ast(ast: ast::Ast) -> Result<Self, InstanceError> {
        let variables = ast
            .variables
            .into_iter()
            .map(|(id, variable)| {
                let variable = ast::Variable {
                    domain: variable.node.domain,
                    value: variable.node.value,
                    annotations: map_annotations(&variable.node.annotations)?,
                };

                Ok((id, variable))
            })
            .collect::<Result<_, _>>()?;

        let arrays = ast
            .arrays
            .into_iter()
            .map(|(id, array)| {
                let array = ast::Array {
                    domain: array.node.domain,
                    contents: array.node.contents,
                    annotations: map_annotations(&array.node.annotations)?,
                };

                Ok((id, array))
            })
            .collect::<Result<_, _>>()?;

        let constraints = ast
            .constraints
            .iter()
            .map(|constraint| {
                let annotations = map_annotations(&constraint.node.annotations)?;

                let instance_constraint = TConstraint::from_ast(constraint)?;

                Ok(AnnotatedConstraint {
                    constraint: ast::Node {
                        node: instance_constraint,
                        span: constraint.span,
                    },
                    annotations,
                })
            })
            .collect::<Result<_, _>>()?;

        let solve = Solve {
            method: match ast.solve.method.node {
                ast::Method::Satisfy => ast::Node {
                    node: Method::Satisfy,
                    span: ast.solve.method.span,
                },
                ast::Method::Optimize {
                    direction,
                    objective,
                } => ast::Node {
                    node: Method::Optimize {
                        direction,
                        objective: <VariableExpr<Int> as FromLiteral>::from_literal(&ast::Node {
                            node: objective,
                            span: ast.solve.method.span,
                        })?,
                    },
                    span: ast.solve.method.span,
                },
            },
            annotations: map_annotations(&ast.solve.annotations)?,
        };

        Ok(TypedInstance {
            variables,
            arrays,
            constraints,
            solve,
        })
    }
}

fn map_annotations<Ann: FlatZincAnnotation>(
    annotations: &[ast::Node<ast::Annotation>],
) -> Result<Vec<ast::Node<Ann>>, InstanceError> {
    annotations
        .iter()
        .filter_map(|annotation| {
            Ann::from_ast(annotation)
                .map(|maybe_node| {
                    maybe_node.map(|node| ast::Node {
                        node,
                        span: annotation.span,
                    })
                })
                .transpose()
        })
        .collect()
}
