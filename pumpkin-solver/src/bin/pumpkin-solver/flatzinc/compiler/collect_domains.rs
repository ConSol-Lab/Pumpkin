//! Compilation phase that builds a map from flatzinc variables to solver domains.

use std::rc::Rc;

use flatzinc::Annotation;
use pumpkin_core::containers::HashMap;
use pumpkin_core::variables::DomainId;
use pumpkin_core::Solver;
use pumpkin_solver::variables::Literal;

use super::context::CompilationContext;
use super::context::Domain;
use crate::flatzinc::ast::FlatZincAst;
use crate::flatzinc::ast::SingleVarDecl;
use crate::flatzinc::instance::Output;
use crate::flatzinc::FlatZincError;

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
                        create_integer_domain(
                            context.solver,
                            &mut context.constant_domain_ids,
                            representative,
                            domain,
                        )
                    });

                let literal = Literal::new(domain_id);

                if is_output_variable(annos) {
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
                        create_integer_domain(
                            context.solver,
                            &mut context.constant_domain_ids,
                            representative,
                            domain,
                        )
                    });

                if is_output_variable(annos) {
                    context.outputs.push(Output::int(id, domain_id));
                }
            }
        }
    }

    Ok(())
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
