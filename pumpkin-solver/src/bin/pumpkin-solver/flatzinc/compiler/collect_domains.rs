//! Compilation phase that builds a map from flatzinc variables to solver domains.

use std::rc::Rc;

use flatzinc::Annotation;
use pumpkin_core::Solver;
use pumpkin_core::containers::HashMap;
use pumpkin_core::variables::DomainId;
use pumpkin_solver::core::predicate;
use pumpkin_solver::core::predicates::Predicate;

use super::context::CompilationContext;
use super::context::Domain;
use crate::flatzinc::FlatZincError;
use crate::flatzinc::ast::FlatZincAst;
use crate::flatzinc::ast::SingleVarDecl;
use crate::flatzinc::instance::Output;

pub(crate) fn run(
    ast: &FlatZincAst,
    context: &mut CompilationContext,
) -> Result<(), FlatZincError> {
    for single_var_decl in &ast.single_variables {
        match single_var_decl {
            SingleVarDecl::Bool { id, annos, .. } => {
                let id = context.identifiers.get_interned(id);

                let predicate = get_or_create_predicate(context, &id);

                if is_output_variable(annos) {
                    context.outputs.push(Output::bool(id, predicate));
                }
            }

            SingleVarDecl::IntInRange { id, annos, .. }
            | SingleVarDecl::IntInSet {
                id, set: _, annos, ..
            } => {
                let id = context.identifiers.get_interned(id);

                let domain_id = get_or_create_domain(context, &id);

                if is_output_variable(annos) {
                    context.outputs.push(Output::int(id, domain_id));
                }
            }
        }
    }

    Ok(())
}

/// Returns the predicate for the given boolean variable, which is either defined over a comparison
/// (see [`CompilationContext::boolean_predicates`]) or over the domain of the boolean.
fn get_or_create_predicate(context: &mut CompilationContext, id: &str) -> Predicate {
    let representative = context.equivalences.representative(id);

    if let Some(predicate) = context.boolean_predicates.get(&representative) {
        return *predicate;
    }

    let domain_id = get_or_create_domain(context, id);
    predicate![domain_id >= 1]
}

pub(crate) fn get_or_create_domain(context: &mut CompilationContext, id: &str) -> DomainId {
    let representative = context.equivalences.representative(id);
    let domain = context.equivalences.domain(id);

    *context
        .variable_map
        .entry(Rc::clone(&representative))
        .or_insert_with(|| {
            create_integer_domain(
                context.solver,
                &mut context.constant_domain_ids,
                representative,
                domain,
            )
        })
}

fn create_integer_domain(
    solver: &mut Solver,
    constant_domains: &mut HashMap<i32, DomainId>,
    identifier: Rc<str>,
    domain: Domain,
) -> DomainId {
    if domain.is_constant() {
        let value = match &domain {
            Domain::IntervalDomain { lb, ub: _ } => *lb,
            Domain::SparseDomain { values } => values[0],
        };

        *constant_domains
            .entry(value)
            .or_insert_with(|| domain.into_variable(solver, value.to_string()))
    } else {
        domain.into_variable(solver, identifier.to_string())
    }
}

fn is_output_variable(annos: &[Annotation]) -> bool {
    annos.iter().any(|ann| ann.id == "output_var")
}
