mod collect_domains;
mod context;
mod create_objective;
mod create_search_strategy;
mod define_constants;
mod define_variable_arrays;
mod handle_set_in;
mod merge_equivalences;
mod post_constraints;
mod prepare_variables;
mod remove_unused_variables;
mod reserve_constraint_tags;

use context::CompilationContext;
use pumpkin_solver::Solver;

use super::ast::FlatZincAst;
use super::instance::FlatZincInstance;
use super::FlatZincError;
use super::FlatZincOptions;

pub(crate) fn compile(
    mut ast: FlatZincAst,
    solver: &mut Solver,
    options: FlatZincOptions,
) -> Result<FlatZincInstance, FlatZincError> {
    let mut context = CompilationContext::new(solver);

    define_constants::run(&ast, &mut context)?;
    reserve_constraint_tags::run(&ast, &mut context)?;
    remove_unused_variables::run(&mut ast, &mut context)?;
    prepare_variables::run(&ast, &mut context)?;
    merge_equivalences::run(&mut ast, &mut context, &options)?;
    handle_set_in::run(&mut context)?;
    collect_domains::run(&ast, &mut context)?;
    define_variable_arrays::run(&ast, &mut context)?;
    post_constraints::run(&ast, &mut context, &options)?;
    let objective_function = create_objective::run(&ast, &mut context)?;
    let search = create_search_strategy::run(&ast, &mut context, objective_function, options)?;

    Ok(FlatZincInstance {
        outputs: context.outputs,
        objective_function,
        search: Some(search),
    })
}
