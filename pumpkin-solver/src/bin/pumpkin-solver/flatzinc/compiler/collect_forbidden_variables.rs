//! Compilation phase that collects the variables annotated with `is_defined_var`, which the solver
//! never branches on and which no learned nogood contains.

use pumpkin_solver::core::containers::HashMap;
use pumpkin_solver::core::variables::DomainId;

use super::context::CompilationContext;
use crate::flatzinc::FlatZincError;
use crate::flatzinc::FlatZincOptions;
use crate::flatzinc::ast::FlatZincAst;
use crate::flatzinc::ast::SingleVarDecl;
use crate::flatzinc::instance::FlatzincObjective;

pub(crate) fn run(
    ast: &FlatZincAst,
    context: &mut CompilationContext,
    objective: Option<FlatzincObjective>,
    options: &FlatZincOptions,
) -> Result<(), FlatZincError> {
    if !options.eliminate_defined_variables {
        return Ok(());
    }

    // Identifiers can be merged into one domain; a domain is forbidden only if all of them are
    // annotated, since otherwise the search may need to branch on it.
    let mut are_all_identifiers_defined: HashMap<DomainId, bool> = HashMap::default();

    for single_var_decl in &ast.single_variables {
        let (SingleVarDecl::Bool { id, annos, .. }
        | SingleVarDecl::IntInRange { id, annos, .. }
        | SingleVarDecl::IntInSet { id, annos, .. }) = single_var_decl;

        let representative = context.equivalences.representative(id);
        let Some(&domain_id) = context.variable_map.get(&representative) else {
            continue;
        };

        let is_defined = annos
            .iter()
            .any(|annotation| annotation.id == "is_defined_var");
        let are_defined = are_all_identifiers_defined.entry(domain_id).or_insert(true);
        *are_defined &= is_defined;
    }

    // The optimisation procedures post decisions over the objective variable.
    let objective_variable = objective.map(|objective| match objective {
        FlatzincObjective::Maximize(domain_id) | FlatzincObjective::Minimize(domain_id) => {
            domain_id
        }
    });

    context.forbidden_variables = are_all_identifiers_defined
        .into_iter()
        .filter(|&(domain_id, are_defined)| are_defined && Some(domain_id) != objective_variable)
        .map(|(domain_id, _)| domain_id)
        .collect();

    Ok(())
}
