mod apply_preamble;
mod check_no_variable_aliases;
mod collect_domains;
mod collect_views;
mod context;
mod create_objective;
mod create_search_strategy;
mod define_constants;
mod define_variable_arrays;
mod handle_set_in;
mod name_rules;
mod post_constraints;
mod prepare_variables;
mod remove_unused_variables;
mod reserve_constraint_tags;
mod warn_default_bounds;

use context::CompilationContext;
pub use context::ViewStatistics;
use pumpkin_solver::Solver;

use super::FlatZincError;
use super::FlatZincOptions;
use super::ast::FlatZincAst;
use super::instance::FlatZincInstance;
use crate::flatzinc::ProofFormat;

pub(crate) fn compile(
    mut ast: FlatZincAst,
    solver: &mut Solver,
    options: FlatZincOptions,
) -> Result<(FlatZincInstance, ViewStatistics), FlatZincError> {
    let mut context = CompilationContext::new(solver);
    context.proof_format = options.proof_format;

    if context.solver.is_logging_proof()
        && options.proof_format == ProofFormat::Croc
        && options.all_solutions
        && matches!(ast.solve_item.goal, flatzinc::Goal::Satisfy)
    {
        return Err(FlatZincError::ProofOfSolutionEnumeration);
    }

    define_constants::run(&ast, &mut context)?;
    reserve_constraint_tags::run(&ast, &mut context)?;
    remove_unused_variables::run(&mut ast, &mut context)?;
    prepare_variables::run(&ast, &mut context)?;
    let named_constraints = name_rules::collect(&context);
    match options.proof_format {
        ProofFormat::Croc => match options.preamble.as_ref() {
            Some(preamble) => apply_preamble::run(&ast, &mut context, preamble)?,
            None => collect_views::run(&ast, &mut context, &options)?,
        },
        ProofFormat::Drcp => check_no_variable_aliases::run(&ast, &mut context)?,
    }
    if context.solver.is_logging_proof() && options.proof_format == ProofFormat::Croc {
        warn_default_bounds::run(&ast, &mut context);
    }
    handle_set_in::run(&mut context)?;
    collect_domains::run(&ast, &mut context)?;
    name_rules::run(&mut context, named_constraints)?;
    define_variable_arrays::run(&ast, &mut context)?;
    post_constraints::run(&ast, &mut context, &options)?;
    if let Some(preamble) = options.preamble.as_ref() {
        apply_preamble::check_decompositions(&context, preamble)?;
    }
    let objective_function = create_objective::run(&ast, &mut context)?;
    let search = create_search_strategy::run(&ast, &mut context, objective_function)?;

    if options.writes_preamble {
        let preamble = context.preamble.render();
        context
            .solver
            .log_scaffold_preamble(&preamble)
            .map_err(|_| FlatZincError::InvalidIdentifier {
                identifier: "the scaffold".into(),
                expected_type: "writable proof file".into(),
            })?;
    }

    Ok((
        FlatZincInstance {
            outputs: context.outputs,
            objective_function,
            search: Some(search),
        },
        context.view_statistics,
    ))
}
