pub mod ast;

#[cfg(feature = "fzn")]
mod fzn;

use std::collections::BTreeMap;
use std::rc::Rc;

use ast::Array;
use ast::SolveObjective;
use ast::Variable;

#[derive(Clone, Debug)]
pub struct Instance<InstanceConstraint, ConstraintAnn = (), VariableAnn = ()> {
    /// The variables that are in the instance.
    ///
    /// The key is the identifier of the variable, and the value is the domain of the variable.
    pub variables: BTreeMap<Rc<str>, Variable<VariableAnn>>,

    /// The constraints in the instance.
    pub constraints: Vec<Constraint<InstanceConstraint, ConstraintAnn>>,

    /// The solve item indicating the type of model.
    pub solve: SolveObjective,
}

#[derive(Clone, Debug)]
pub struct Constraint<InstanceConstraint, ConstraintAnn> {
    pub constraint: InstanceConstraint,
    pub annotations: Vec<ConstraintAnn>,
}

#[derive(Clone, Copy, Debug, thiserror::Error)]
#[error("failed to parse constraint from ast")]
pub struct InstanceError;

pub trait FlatZincConstraint: Sized {
    fn from_ast(
        constraint: &ast::Constraint,
        arrays: &BTreeMap<Rc<str>, Array>,
    ) -> Result<Self, InstanceError>;
}

/// Parse an annotation into the instance.
///
/// The difference with [`FlatZincConstraint::from_ast`] is that annotations can be ignored.
/// [`FlatZincAnnotation::from_ast`] can successfully parse an annotation into nothing, signifying
/// the annotation is not of interest in the final [`Instance`].
pub trait FlatZincAnnotation: Sized {
    fn from_ast(annotation: &ast::Annotation) -> Result<Option<Self>, InstanceError>;
}

impl<InstanceConstraint, ConstraintAnn, VariableAnn>
    Instance<InstanceConstraint, ConstraintAnn, VariableAnn>
where
    InstanceConstraint: FlatZincConstraint,
    ConstraintAnn: FlatZincAnnotation,
    VariableAnn: FlatZincAnnotation,
{
    pub fn from_ast(ast: ast::Ast) -> Result<Self, InstanceError> {
        let variables = ast
            .variables
            .into_iter()
            .map(|(id, variable)| {
                let variable = Variable {
                    domain: variable.domain,
                    value: variable.value,
                    annotations: variable
                        .annotations
                        .into_iter()
                        .filter_map(|annotation| VariableAnn::from_ast(&annotation).transpose())
                        .collect::<Result<Vec<VariableAnn>, _>>()?,
                };

                Ok((id, variable))
            })
            .collect::<Result<_, _>>()?;

        let constraints = ast
            .constraints
            .iter()
            .map(|constraint| {
                let annotations = constraint
                    .annotations
                    .iter()
                    .filter_map(|annotation| ConstraintAnn::from_ast(annotation).transpose())
                    .collect::<Result<_, _>>()?;

                let instance_constraint = InstanceConstraint::from_ast(constraint, &ast.arrays)?;

                Ok(Constraint {
                    constraint: instance_constraint,
                    annotations,
                })
            })
            .collect::<Result<_, _>>()?;

        Ok(Instance {
            variables,
            constraints,
            solve: ast.solve,
        })
    }
}
