pub mod ast;

mod error;
mod from_ast;
#[cfg(feature = "fzn")]
mod fzn;

use std::collections::BTreeMap;
use std::rc::Rc;

use ast::SolveObjective;
use ast::Variable;
pub use error::*;
pub use from_ast::*;

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
    pub constraint: ast::Node<InstanceConstraint>,
    pub annotations: Vec<ast::Node<ConstraintAnn>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum IntVariable {
    Identifier(Rc<str>),
    Constant(i64),
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
                    domain: variable.node.domain,
                    value: variable.node.value,
                    annotations: map_annotations(&variable.node.annotations)?,
                };

                Ok((id, variable))
            })
            .collect::<Result<_, _>>()?;

        let constraints = ast
            .constraints
            .iter()
            .map(|constraint| {
                let annotations = map_annotations(&constraint.node.annotations)?;

                let instance_constraint =
                    InstanceConstraint::from_ast(&constraint.node, &ast.arrays)?;

                Ok(Constraint {
                    constraint: ast::Node {
                        node: instance_constraint,
                        span: constraint.span,
                    },
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

fn map_annotations<Ann: FlatZincAnnotation>(
    annotations: &[ast::Node<ast::Annotation>],
) -> Result<Vec<ast::Node<Ann>>, InstanceError> {
    annotations
        .iter()
        .filter_map(|annotation| {
            Ann::from_ast(&annotation.node)
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
