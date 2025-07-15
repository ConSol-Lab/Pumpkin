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

pub(crate) use context::CompilationContext;
use pumpkin_solver::Solver;

use super::instance::FlatZincInstance;
use super::FlatZincError;
use super::FlatZincOptions;

pub(crate) fn compile(
    mut ast: fzn_rs::ast::Ast,
    solver: &mut Solver,
    options: FlatZincOptions,
) -> Result<FlatZincInstance, FlatZincError> {
    let mut context = CompilationContext::new(solver);

    remove_unused_variables::run(&mut ast)?;

    let mut typed_ast = super::ast::Instance::from_ast(ast).expect("handle errors");
    reserve_constraint_tags::run(&mut typed_ast, &mut context)?;

    prepare_variables::run(&typed_ast, &mut context)?;
    merge_equivalences::run(&mut typed_ast, &mut context, &options)?;
    handle_set_in::run(&mut typed_ast, &mut context)?;
    collect_domains::run(&typed_ast, &mut context)?;
    post_constraints::run(&typed_ast, &mut context, &options)?;
    let objective_function = create_objective::run(&ast, &mut context)?;
    let search = create_search_strategy::run(&ast, &mut context, objective_function)?;

    Ok(FlatZincInstance {
        outputs: context.outputs,
        objective_function,
        search: Some(search),
    })
}
