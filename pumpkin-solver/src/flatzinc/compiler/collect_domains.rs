//! Compilation phase that builds a map from flatzinc variables to solver domains.

use std::rc::Rc;

use flatzinc::Annotation;
use pumpkin_core::Solver;
use pumpkin_core::variables::DomainId;

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
                let representative = context.equivalences.representative(&id);
                let domain = context.equivalences.domain(&id);

                let domain_id = *context
                    .variable_map
                    .entry(Rc::clone(&representative))
                    .or_insert_with(|| {
                        create_integer_domain(context.solver, representative, domain)
                    });

                if is_output_variable(annos) {
                    let _ = domain_id;
                    let literal = context.resolve_bool_variable_from_identifier(&id)?;
                    context.outputs.push(Output::bool(id, literal));
                }
            }

            SingleVarDecl::IntInRange { id, annos, .. }
            | SingleVarDecl::IntInSet {
                id, set: _, annos, ..
            } => {
                let id = context.identifiers.get_interned(id);

                let representative = context.equivalences.representative(&id);
                let domain = context.equivalences.domain(&id);

                let domain_id = *context
                    .variable_map
                    .entry(Rc::clone(&representative))
                    .or_insert_with(|| {
                        create_integer_domain(context.solver, representative, domain)
                    });

                if is_output_variable(annos) {
                    let _ = domain_id;
                    let view = context.resolve_integer_variable_from_identifier(&id)?;
                    context.outputs.push(Output::int(id, view));
                }
            }
        }
    }

    Ok(())
}

/// A model variable keeps a domain of its own and its name even when its domain is a single
/// value, so that a proof can state what it knows about it: the constraints a proof checker
/// reads mention the variable, not the value.
fn create_integer_domain(solver: &mut Solver, identifier: Rc<str>, domain: Domain) -> DomainId {
    domain.into_variable(solver, identifier.to_string())
}

fn is_output_variable(annos: &[Annotation]) -> bool {
    annos.iter().any(|ann| ann.id == "output_var")
}
