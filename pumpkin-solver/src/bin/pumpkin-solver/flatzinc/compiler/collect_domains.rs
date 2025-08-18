//! Compilation phase that builds a map from flatzinc variables to solver domains.

use std::rc::Rc;

use fzn_rs::ast;
use pumpkin_core::variables::Literal;

use super::context::CompilationContext;
use super::context::Domain;
use crate::flatzinc::ast::Instance;
use crate::flatzinc::ast::VariableAnnotations;
use crate::flatzinc::instance::Output;
use crate::flatzinc::FlatZincError;

pub(crate) fn run(
    instance: &Instance,
    context: &mut CompilationContext,
) -> Result<(), FlatZincError> {
    for (name, variable) in &instance.variables {
        match &variable.domain.node {
            ast::Domain::Bool => {
                let representative = context.equivalences.representative(name)?;
                let domain = context.equivalences.domain(name);

                let domain_id = *context
                    .variable_map
                    .entry(representative)
                    .or_insert_with(|| domain.into_variable(context.solver, name.to_string()));

                let literal = Literal::new(domain_id);

                if is_output_variable(variable) {
                    context.outputs.push(Output::bool(Rc::clone(name), literal));
                }
            }

            ast::Domain::Int(_) => {
                let representative = context.equivalences.representative(name)?;
                let domain = context.equivalences.domain(name);

                let domain_id = *context
                    .variable_map
                    .entry(Rc::clone(&representative))
                    .or_insert_with(|| {
                        if domain.is_constant() {
                            *context
                                .constant_domain_ids
                                .entry(match &domain {
                                    Domain::IntervalDomain { lb, ub: _ } => *lb,
                                    Domain::SparseDomain { values } => values[0],
                                })
                                .or_insert_with(|| {
                                    domain.into_variable(context.solver, name.to_string())
                                })
                        } else {
                            domain.into_variable(context.solver, name.to_string())
                        }
                    });

                if is_output_variable(variable) {
                    context
                        .outputs
                        .push(Output::int(Rc::clone(name), domain_id));
                }
            }

            ast::Domain::UnboundedInt => {
                return Err(FlatZincError::UnsupportedVariable(name.as_ref().into()))
            }
        }
    }

    Ok(())
}

fn is_output_variable(variable: &ast::Variable<VariableAnnotations>) -> bool {
    variable
        .annotations
        .iter()
        .any(|ann| matches!(ann.node, VariableAnnotations::OutputVar))
}
