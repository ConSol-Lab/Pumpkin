mod collect_domains;
mod constant_comparison;
mod context;
mod create_objective;
mod create_search_strategy;
mod define_constants;
mod define_reified_predicates;
mod define_variable_arrays;
mod handle_set_in;
mod merge_equivalences;
mod post_constraints;
mod prepare_variables;
mod remove_unused_variables;
mod reserve_constraint_tags;

use context::CompilationContext;
use pumpkin_solver::Solver;

use super::FlatZincError;
use super::FlatZincOptions;
use super::ast::FlatZincAst;
use super::instance::FlatZincInstance;

/// Compiles the FlatZinc AST into the solver.
///
/// Before any constraint is posted, the model is preprocessed by the following passes, in order:
/// 1. [`define_constants`] stores the parameter declarations as constants.
/// 2. [`reserve_constraint_tags`] reserves a constraint tag for every FlatZinc constraint and
///    collects the constraints in [`CompilationContext::constraints`]. This happens first so that
///    the first n tags map to the FlatZinc constraints, even if posting a constraint introduces
///    inferences in the proof. Later passes remove constraints from that list, not from the AST.
/// 3. [`remove_unused_variables`] removes the variables which do not occur in any constraint, array
///    or the objective, unless they are output variables.
/// 4. [`prepare_variables`] creates an equivalence class with an initial domain for every variable;
///    a variable which is assigned a constant gets a fixed domain.
/// 5. [`merge_equivalences`] merges the equivalence classes of variables which are assigned to each
///    other (`var 1..5: b = a;`) or linked by an `int_eq` or `bool2int` constraint. These
///    constraints are removed, except when logging a full proof since they would be missing from
///    it.
/// 6. [`handle_set_in`] intersects the domain of a variable with the set of a `set_in` constraint.
/// 7. [`define_reified_predicates`] defines a boolean which reifies a comparison between an integer
///    variable and a constant (e.g. `int_le_reif(x, 5, r)`) as the predicate of that comparison
///    (`[x <= 5]`), and removes the defining constraint. This runs after the equivalences are
///    merged, as it is decided per equivalence class, and is skipped when logging a full proof.
/// 8. [`collect_domains`] creates a solver domain for every remaining equivalence class. Booleans
///    defined in the previous pass are resolved to their predicate and get no domain of their own.
/// 9. [`define_variable_arrays`] resolves the variable arrays to the created predicates and
///    domains.
///
/// Afterwards, the remaining constraints are posted, and the objective and search strategy are
/// created.
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
    merge_equivalences::run(&ast, &mut context, &options)?;
    handle_set_in::run(&mut context)?;
    define_reified_predicates::run(&mut context, &options)?;
    collect_domains::run(&ast, &mut context)?;
    define_variable_arrays::run(&ast, &mut context)?;
    post_constraints::run(&ast, &mut context, &options)?;
    let objective_function = create_objective::run(&ast, &mut context)?;
    let search = create_search_strategy::run(&ast, &mut context, objective_function)?;

    Ok(FlatZincInstance {
        outputs: context.outputs,
        objective_function,
        search: Some(search),
    })
}
