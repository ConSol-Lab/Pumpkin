//! Compilation phase that builds a map from flatzinc variables to solver domains.

use std::rc::Rc;

use fzn_rs::ast;
use fzn_rs::FromLiteral;
use fzn_rs::VariableExpr;

use super::context::CompilationContext;
use super::context::Domain;
use crate::flatzinc::ast::ArrayAnnotations;
use crate::flatzinc::ast::Instance;
use crate::flatzinc::ast::VariableAnnotations;
use crate::flatzinc::instance::Output;
use crate::flatzinc::FlatZincError;

pub(crate) fn run(
    instance: &Instance,
    context: &mut CompilationContext,
) -> Result<(), FlatZincError> {
    for (name, array) in &instance.arrays {
        #[allow(
            clippy::unnecessary_find_map,
            reason = "it is only unnecessary because ArrayAnnotations has one variant"
        )]
        let Some(shape) = array.annotations.iter().find_map(|ann| match &ann.node {
            ArrayAnnotations::OutputArray(shape) => Some(shape),
        }) else {
            continue;
        };

        // This is a bit hacky. We do not know easily whether the array is an array of
        // integers or booleans. So we try to resolve both, and then see which one works.

        let bool_array = array
            .contents
            .iter()
            .map(|node| {
                let variable = <VariableExpr<bool> as FromLiteral>::from_literal(node)?;

                let literal = context.resolve_bool_variable(&variable)?;
                Ok(literal)
            })
            .collect::<Result<Vec<_>, FlatZincError>>();

        let int_array = array
            .contents
            .iter()
            .map(|node| {
                let variable = <VariableExpr<i32> as FromLiteral>::from_literal(node)?;

                let domain_id = context.resolve_integer_variable(&variable)?;
                Ok(domain_id)
            })
            .collect::<Result<Vec<_>, FlatZincError>>();

        let output = match (bool_array, int_array) {
            (Ok(_), Ok(_)) => {
                unreachable!("Array of identifiers that are both integers and booleans")
            }

            (Ok(bools), Err(_)) => Output::array_of_bool(Rc::clone(name), shape.clone(), bools),
            (Err(_), Ok(ints)) => Output::array_of_int(Rc::clone(name), shape.clone(), ints),

            (Err(_), Err(_)) => unreachable!("Array is neither of boolean or integer variables."),
        };

        context.outputs.push(output);
    }

    for (name, variable) in &instance.variables {
        match &variable.domain.node {
            ast::Domain::Bool => {
                let representative = context.literal_equivalences.representative(name);
                let domain = context.literal_equivalences.domain(name);

                let literal = *context
                    .boolean_variable_map
                    .entry(representative)
                    .or_insert_with(|| domain.into_boolean(context.solver, name.to_string()));

                if is_output_variable(variable) {
                    context.outputs.push(Output::bool(Rc::clone(name), literal));
                }
            }

            ast::Domain::Int(_) => {
                let representative = context.integer_equivalences.representative(name);
                let domain = context.integer_equivalences.domain(name);

                let domain_id = *context
                    .integer_variable_map
                    .entry(representative)
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
