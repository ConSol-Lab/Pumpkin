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
    let literal_booleans: std::collections::HashSet<Rc<str>> = context
        .literal_predicates
        .iter()
        .map(|literal| Rc::clone(&literal.boolean))
        .collect();
    let mut literal_outputs = vec![];
    for single_var_decl in &ast.single_variables {
        match single_var_decl {
            SingleVarDecl::Bool { id, annos, .. } => {
                let id = context.identifiers.get_interned(id);
                // Made the literal of its predicate once the variables exist.
                if literal_booleans.contains(&id) {
                    if is_output_variable(annos) {
                        literal_outputs.push(Rc::clone(&id));
                    }
                    continue;
                }

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

    // A boolean that stands for a comparison is the solver's literal for its predicate, tied to
    // it by the clauses the solver posts under the comparison's tag; the proof writes its atoms
    // as the predicate's.
    for literal in context.literal_predicates.clone() {
        let domain = context.variable_map[&literal.variable];
        let predicate = match literal.comparator {
            "<=" => pumpkin_core::predicate![domain <= literal.value],
            ">=" => pumpkin_core::predicate![domain >= literal.value],
            "==" => pumpkin_core::predicate![domain == literal.value],
            _ => pumpkin_core::predicate![domain != literal.value],
        };
        let boolean = context.solver.new_named_literal_for_predicate(
            predicate,
            literal.tag,
            literal.boolean.to_string(),
        );
        // Once the boolean is the atom, the clauses are tautologies, and what an inference from
        // them needs beyond its premises is the bounds of the compared variable.
        context
            .solver
            .name_rule_of_constraint(literal.tag, "initial_domain");
        let _ = context.variable_map.insert(
            Rc::clone(&literal.boolean),
            boolean.get_true_predicate().get_domain(),
        );
        let _ = context
            .literal_booleans
            .insert(Rc::clone(&literal.boolean), boolean);
        if literal_outputs.contains(&literal.boolean) {
            context
                .outputs
                .push(Output::bool(Rc::clone(&literal.boolean), boolean));
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
