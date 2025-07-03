//! Compilation phase that builds a map from flatzinc variables to solver domains.

use std::rc::Rc;

use flatzinc::Annotation;
use pumpkin_solver::variables::Literal;

use super::context::CompilationContext;
use super::context::Domain;
use crate::flatzinc::ast::FlatZincAst;
use crate::flatzinc::ast::SingleVarDecl;
use crate::flatzinc::instance::Output;
use crate::flatzinc::FlatZincError;

fn replace_bool2int_identifiers_with_representatives(context: &mut CompilationContext) {
    let entries = context
        .bool2int
        .iter()
        .map(|(bool_id, int_id)| (Rc::clone(bool_id), Rc::clone(int_id)))
        .collect::<Vec<_>>();
    context.bool2int.clear();
    for (bool_id, int_id) in entries {
        let _ = context.bool2int.insert(
            context.literal_equivalences.representative(&bool_id),
            int_id,
        );
    }
}

pub(crate) fn run(
    ast: &FlatZincAst,
    context: &mut CompilationContext,
) -> Result<(), FlatZincError> {
    replace_bool2int_identifiers_with_representatives(context);

    for single_var_decl in &ast.single_variables {
        match single_var_decl {
            SingleVarDecl::Bool { id, annos, .. } => {
                let id = context.identifiers.get_interned(id);

                let representative = context.literal_equivalences.representative(&id);
                let domain = context.literal_equivalences.domain(&id);

                let literal = *context
                    .boolean_variable_map
                    .entry(Rc::clone(&representative))
                    .or_insert_with(|| {
                        if let Some(int_id) = context.bool2int.get(&representative) {
                            let int_representative =
                                context.integer_equivalences.representative(int_id);
                            let int_domain = context.integer_equivalences.domain(int_id);
                            context.integer_equivalences.binarise(int_id);
                            let domain_id = *context
                                .integer_variable_map
                                .entry(int_representative)
                                .or_insert_with(|| {
                                    if int_domain.is_constant() {
                                        let result = *context
                                            .constant_domain_ids
                                            .entry(match &int_domain {
                                                Domain::IntervalDomain { lb, ub: _ } => *lb,
                                                Domain::SparseDomain { values } => values[0],
                                            })
                                            .or_insert_with(|| {
                                                int_domain.into_variable(
                                                    context.solver,
                                                    int_id.to_string(),
                                                )
                                            });
                                        result
                                    } else {
                                        int_domain.into_variable(context.solver, int_id.to_string())
                                    }
                                });
                            Literal::new(domain_id)
                        } else {
                            domain.into_boolean(context.solver, id.to_string())
                        }
                    });

                if is_output_variable(annos) {
                    context.outputs.push(Output::bool(id, literal));
                }
            }

            SingleVarDecl::IntInRange { id, annos, .. } => {
                let id = context.identifiers.get_interned(id);

                let representative = context.integer_equivalences.representative(&id);
                let domain = context.integer_equivalences.domain(&id);

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
                                    domain.into_variable(context.solver, id.to_string())
                                })
                        } else {
                            domain.into_variable(context.solver, id.to_string())
                        }
                    });

                if is_output_variable(annos) {
                    context.outputs.push(Output::int(id, domain_id));
                }
            }

            SingleVarDecl::IntInSet { id, set, annos, .. } => {
                let id = context.identifiers.get_interned(id);

                let domain_id = if set.len() == 1 {
                    let value = i32::try_from(set[0])?;

                    *context.constant_domain_ids.entry(value).or_insert_with(|| {
                        context
                            .solver
                            .new_named_bounded_integer(value, value, id.to_string())
                    })
                } else {
                    let values = set
                        .iter()
                        .copied()
                        .map(i32::try_from)
                        .collect::<Result<Vec<_>, _>>()?;

                    let domain_id = context
                        .solver
                        .new_named_sparse_integer(values, id.to_string());
                    let _ = context
                        .integer_variable_map
                        .insert(Rc::clone(&id), domain_id);

                    domain_id
                };

                if is_output_variable(annos) {
                    context.outputs.push(Output::int(id, domain_id));
                }
            }
        }
    }

    Ok(())
}

fn is_output_variable(annos: &[Annotation]) -> bool {
    annos.iter().any(|ann| ann.id == "output_var")
}
