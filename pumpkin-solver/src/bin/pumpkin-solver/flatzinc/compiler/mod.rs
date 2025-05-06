mod collect_domains;
mod configure_mdd_constraints;
mod context;
mod create_objective;
mod create_search_strategy;
mod define_constants;
mod define_variable_arrays;
mod handle_set_in;
mod merge_equivalences;
mod post_constraints;
mod prepare_variables;

use context::CompilationContext;
use log::warn;
use pumpkin_solver::options::DecisionDiagramOptions;
use pumpkin_solver::Solver;

use super::ast::FlatZincAst;
use super::instance::FlatZincInstance;
use super::FlatZincError;
use super::FlatZincOptions;

pub(crate) fn compile(
    ast: FlatZincAst,
    solver: &mut Solver,
    options: FlatZincOptions,
    dd_options: Option<DecisionDiagramOptions>,
) -> Result<FlatZincInstance, FlatZincError> {
    let mut context = CompilationContext::new(solver);

    define_constants::run(&ast, &mut context)?;
    prepare_variables::run(&ast, &mut context)?;
    merge_equivalences::run(&ast, &mut context)?;
    handle_set_in::run(&ast, &mut context)?;
    collect_domains::run(&ast, &mut context)?;
    define_variable_arrays::run(&ast, &mut context)?;
    post_constraints::run(&ast, &mut context, options, dd_options.is_some())?;
    if let Some(dd_options) = dd_options {
        match configure_mdd_constraints::run(&mut context, dd_options) {
            Ok(_) => {}
            Err(_) => {
                warn!("Failed to compile the decision diagram constraints, proceeding to solve the model without them");
            }
        }
    }
    let objective_function = create_objective::run(&ast, &mut context)?;
    let search = create_search_strategy::run(&ast, &mut context)?;

    Ok(FlatZincInstance {
        outputs: context.outputs,
        objective_function,
        search: Some(search),
    })
}
