//! Compilation phase that builds a map from flatzinc variables to solver domains.

use std::rc::Rc;

use flatzinc::Annotation;
use pumpkin_core::Solver;
use pumpkin_core::containers::HashMap;
use pumpkin_core::variables::DomainId;
use pumpkin_solver::core::predicate;
use pumpkin_solver::core::variables::Literal;

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
    // The literals of booleans defined over a comparison need the domain of the integer variable,
    // so the integer domains are created first. Integers in an equivalence class which is defined
    // as a literal are a view of that literal, and do not get a domain (see
    // `CompilationContext::resolve_integer_view`).
    for single_var_decl in &ast.single_variables {
        if let SingleVarDecl::IntInRange { id, .. } | SingleVarDecl::IntInSet { id, .. } =
            single_var_decl
        {
            let id = context.identifiers.get_interned(id);
            if !is_defined_as_literal(context, &id) {
                let _ = get_or_create_domain(context, &id);
            }
        }
    }

    for single_var_decl in &ast.single_variables {
        match single_var_decl {
            SingleVarDecl::Bool { id, annos, .. } => {
                let id = context.identifiers.get_interned(id);

                let literal = get_or_create_literal(context, &id)?;

                if is_output_variable(annos) {
                    context.outputs.push(Output::bool(id, literal));
                }
            }

            SingleVarDecl::IntInRange { id, annos, .. }
            | SingleVarDecl::IntInSet {
                id, set: _, annos, ..
            } => {
                let id = context.identifiers.get_interned(id);

                if !is_defined_as_literal(context, &id) {
                    let domain_id = get_or_create_domain(context, &id);

                    if is_output_variable(annos) {
                        context.outputs.push(Output::int(id, domain_id));
                    }
                } else if is_output_variable(annos) {
                    // The output needs a domain, which is created from the literal.
                    let _ = get_or_create_literal(context, &id)?;
                    let domain_id = context.resolve_integer_variable_from_identifier(&id)?;
                    context.outputs.push(Output::int(id, domain_id));
                }
            }
        }
    }

    Ok(())
}

/// Returns the literal for the given boolean variable, which is either defined over a comparison
/// (see [`CompilationContext::reified_literals`]) or over the domain of the boolean.
fn get_or_create_literal(
    context: &mut CompilationContext,
    id: &str,
) -> Result<Literal, FlatZincError> {
    let representative = context.equivalences.representative(id);

    if let Some(literal) = context.boolean_literals.get(&representative) {
        return Ok(*literal);
    }

    if let Some(comparison) = context.reified_literals.get(&representative).cloned() {
        let literal = comparison.to_literal(context)?;
        let _ = context.boolean_literals.insert(representative, literal);
        return Ok(literal);
    }

    let domain_id = get_or_create_domain(context, id);
    Ok(Literal::new(predicate![domain_id >= 1]))
}

fn is_defined_as_literal(context: &CompilationContext, id: &str) -> bool {
    context
        .reified_literals
        .contains_key(&context.equivalences.representative(id))
}

fn get_or_create_domain(context: &mut CompilationContext, id: &str) -> DomainId {
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
