//! Scan through all constraint definition and determine whether a `set_in` constraint is present;
//! is this is the case then update the domain of the variable directly.
use std::rc::Rc;

use fzn_rs::VariableExpr;

use super::context::CompilationContext;
use crate::flatzinc::{ast::Instance, constraints::Constraints, error::FlatZincError};

pub(crate) fn run(
    instance: &mut Instance,
    context: &mut CompilationContext,
) -> Result<(), FlatZincError> {
    for constraint in &instance.constraints {
        let (variable, set) = match &constraint.constraint.node {
            Constraints::SetIn(variable, set) => (variable, set),
            _ => continue,
        };

        let id = match variable {
            VariableExpr::Identifier(id) => Rc::clone(&id),
            _ => return Err(FlatZincError::UnexpectedExpr),
        };

        let mut domain = context.integer_equivalences.get_mut_domain(&id);

        // We take the intersection between the two domains
        let new_domain = domain.merge(&set.into());
        *domain = new_domain;
    }

    Ok(())
}
