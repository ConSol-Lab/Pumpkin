mod collect_domains;
mod constraints;
mod context;
mod create_objective;
mod define_constants;
mod define_variable_arrays;
mod merge_equivalences;
mod post_constraints;
mod prepare_variables;

use context::CompilationContext;
use pumpkin_lib::engine::ConstraintSatisfactionSolver;

use super::ast::FlatZincAst;
use super::instance::FlatZincInstance;
use super::FlatZincError;

pub fn compile(
    ast: FlatZincAst,
    solver: &mut ConstraintSatisfactionSolver,
) -> Result<FlatZincInstance, FlatZincError> {
    let mut context = CompilationContext::new(solver);

    define_constants::run(&ast, &mut context)?;
    prepare_variables::run(&ast, &mut context)?;
    merge_equivalences::run(&ast, &mut context)?;
    collect_domains::run(&ast, &mut context)?;
    define_variable_arrays::run(&ast, &mut context)?;
    post_constraints::run(&ast, &mut context)?;
    let objective_function = create_objective::run(&ast, &mut context)?;

    Ok(FlatZincInstance {
        outputs: context.outputs,
        objective_function,
    })
}
