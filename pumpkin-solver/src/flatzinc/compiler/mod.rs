mod apply_preamble;
mod breaking_steps;
mod check_no_variable_aliases;
mod collect_domains;
mod collect_views;
mod context;
mod create_objective;
mod create_search_strategy;
mod define_constants;
mod define_variable_arrays;
mod dominance_breaking;
mod full_solution;
mod handle_set_in;
mod name_rules;
mod post_constraints;
mod prepare_variables;
mod remove_unused_variables;
mod reserve_constraint_tags;
mod symmetry_breaking;
mod warn_default_bounds;

use context::CompilationContext;
pub use context::ViewStatistics;

use super::CompilationOptions;
use super::FlatZincError;
use super::ast::FlatZincAst;
use super::instance::FlatZincInstance;
use crate::Solver;
use crate::flatzinc::CrocProofType;
use crate::flatzinc::ProofLogging;

pub(crate) fn compile(
    mut ast: FlatZincAst,
    solver: &mut Solver,
    options: CompilationOptions,
) -> Result<FlatZincInstance, FlatZincError> {
    let mut context = CompilationContext::new(solver);
    context.proof_logging = &options.proof_logging;
    context.full_solution = options.full_solution;

    if context.solver.is_logging_proof()
        && matches!(options.proof_logging, ProofLogging::Croc { .. })
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
    match &options.proof_logging {
        ProofLogging::Disabled
        | ProofLogging::Croc {
            applied_preamble: None,
            ..
        } => collect_views::run(&ast, &mut context, &options)?,
        ProofLogging::Croc {
            applied_preamble: Some(preamble),
            ..
        } => apply_preamble::run(&ast, &mut context, preamble)?,
        ProofLogging::Drcp { .. } => check_no_variable_aliases::run(&ast, &mut context)?,
    }
    if context.solver.is_logging_proof()
        && matches!(options.proof_logging, ProofLogging::Croc { .. })
    {
        warn_default_bounds::run(&ast, &mut context);
    }
    handle_set_in::run(&mut context)?;
    collect_domains::run(&ast, &mut context)?;
    name_rules::run(&mut context, named_constraints)?;
    define_variable_arrays::run(&ast, &mut context)?;
    post_constraints::run(&ast, &mut context, &options)?;
    if let ProofLogging::Croc {
        applied_preamble: Some(preamble),
        ..
    } = &options.proof_logging
    {
        apply_preamble::check_decompositions(&context, preamble)?;
        apply_preamble::apply_breaking(&mut context, preamble)?;
    }
    symmetry_breaking::run(&ast, &mut context, &options)?;
    symmetry_breaking::run_value_symmetry(&ast, &mut context, &options)?;
    dominance_breaking::run(&ast, &mut context, &options)?;
    let objective_function = create_objective::run(&ast, &mut context)?;
    let search = create_search_strategy::run(&ast, &mut context, objective_function)?;
    let full_solution = options
        .full_solution
        .then(|| full_solution::run(&ast, &mut context));

    if let ProofLogging::Croc {
        proof_type: CrocProofType::Scaffold,
        ..
    } = options.proof_logging
    {
        let preamble = context.preamble.render();
        context
            .solver
            .log_scaffold_preamble(&preamble)
            .map_err(|_| FlatZincError::InvalidIdentifier {
                identifier: "the scaffold".into(),
                expected_type: "writable proof file".into(),
            })?;
    }

    Ok(FlatZincInstance {
        outputs: context.outputs,
        objective_function,
        search: Some(search),
        view_statistics: context.view_statistics,
        full_solution,
    })
}
