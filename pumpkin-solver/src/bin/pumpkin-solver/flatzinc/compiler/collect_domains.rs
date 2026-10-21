//! Compilation phase that builds a map from flatzinc variables to solver domains.

use std::rc::Rc;

use flatzinc::Annotation;
use pumpkin_core::Solver;
use pumpkin_core::containers::HashMap;
use pumpkin_core::variables::AnyInteger;
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

                let variable = get_or_create_variable(context, &id);

                if is_output_variable(annos) {
                    context.outputs.push(Output::int(id, variable));
                }
            }
        }
    }

    Ok(())
}

/// Returns the predicate for the given boolean variable (see [`get_or_create_variable`]).
pub(crate) fn get_or_create_predicate(context: &mut CompilationContext, id: &str) -> Predicate {
    let variable = get_or_create_variable(context, id);
    predicate![variable >= 1]
}

/// Returns the variable for the given identifier. This is the predicate which defines the
/// equivalence class of the identifier (see [`CompilationContext::boolean_predicates`]) if there is
/// one; otherwise it is the domain of the equivalence class, which is created if it does not exist
/// yet.
pub(crate) fn get_or_create_variable(context: &mut CompilationContext, id: &str) -> AnyInteger {
    let representative = context.equivalences.representative(id);
    let domain = context.equivalences.domain(id);

    *context
        .variable_map
        .entry(Rc::clone(&representative))
        .or_insert_with(|| match context.boolean_predicates.get(&representative) {
            Some(&predicate) => predicate.into(),
            None => create_integer_domain(
                context.solver,
                &mut context.constant_domain_ids,
                representative,
                domain,
            )
            .into(),
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
