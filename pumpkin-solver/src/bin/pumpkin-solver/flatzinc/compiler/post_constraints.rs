//! Compile constraints into CP propagators

use std::rc::Rc;

use implementation::propagators::cumulative::Task;
use pumpkin_propagators::disjunctive::ArgDisjunctiveTask;
use pumpkin_solver::constraints::Constraint;
use pumpkin_solver::constraints::NegatableConstraint;
use pumpkin_solver::predicate;
use pumpkin_solver::predicates::Predicate;
use pumpkin_solver::proof::ConstraintTag;
use pumpkin_solver::variables::AffineView;
use pumpkin_solver::variables::DomainId;
use pumpkin_solver::variables::TransformableVariable;

use super::context::CompilationContext;
use crate::flatzinc::FlatZincError;
use crate::flatzinc::FlatZincOptions;
use crate::flatzinc::ast::FlatZincAst;
use crate::flatzinc::compiler::context::Set;

pub(crate) fn run(
    _: &FlatZincAst,
    context: &mut CompilationContext,
    options: &FlatZincOptions,
) -> Result<(), FlatZincError> {
    for (constraint_tag, constraint_item) in std::mem::take(&mut context.constraints) {
        let flatzinc::ConstraintItem { id, exprs, annos } = &constraint_item;

        let is_satisfiable: bool = match id.as_str() {
            "pumpkin_disjunctive_strict" => {
                compile_disjunctive_strict(context, exprs, constraint_tag)?
            }
            "array_int_maximum" => compile_array_int_maximum(context, exprs, constraint_tag)?,
            "array_int_minimum" => compile_array_int_minimum(context, exprs, constraint_tag)?,
            "int_max" => compile_ternary_int_predicate(
                context,
                exprs,
                annos,
                "int_max",
                constraint_tag,
                |a, b, c, constraint_tag| pumpkin_constraints::maximum([a, b], c, constraint_tag),
            )?,
            "int_min" => compile_ternary_int_predicate(
                context,
                exprs,
                annos,
                "int_min",
                constraint_tag,
                |a, b, c, constraint_tag| pumpkin_constraints::minimum([a, b], c, constraint_tag),
            )?,

            // We rewrite `array_int_element` to `array_var_int_element`.
            "array_int_element" => compile_array_var_int_element(context, exprs, constraint_tag)?,
            "array_var_int_element" => {
                compile_array_var_int_element(context, exprs, constraint_tag)?
            }

            "int_eq_imp" => compile_binary_int_imp(
                context,
                exprs,
                annos,
                options,
                "int_eq_imp",
                constraint_tag,
                pumpkin_constraints::binary_equals,
            )?,
            "int_ge_imp" => compile_binary_int_imp(
                context,
                exprs,
                annos,
                options,
                "int_ge_imp",
                constraint_tag,
                pumpkin_constraints::binary_greater_than_or_equals,
            )?,
            "int_gt_imp" => compile_binary_int_imp(
                context,
                exprs,
                annos,
                options,
                "int_gt_imp",
                constraint_tag,
                pumpkin_constraints::binary_greater_than,
            )?,
            "int_le_imp" => compile_binary_int_imp(
                context,
                exprs,
                annos,
                options,
                "int_le_imp",
                constraint_tag,
                pumpkin_constraints::binary_less_than_or_equals,
            )?,
            "int_lt_imp" => compile_binary_int_imp(
                context,
                exprs,
                annos,
                options,
                "int_lt_imp",
                constraint_tag,
                pumpkin_constraints::binary_less_than,
            )?,
            "int_ne_imp" => compile_binary_int_imp(
                context,
                exprs,
                annos,
                options,
                "int_ne_imp",
                constraint_tag,
                pumpkin_constraints::binary_not_equals,
            )?,

            "int_lin_eq_imp" => compile_int_lin_imp_predicate(
                context,
                exprs,
                annos,
                options,
                "int_lin_eq_imp",
                constraint_tag,
                pumpkin_constraints::equals,
            )?,
            "int_lin_ge_imp" => compile_int_lin_imp_predicate(
                context,
                exprs,
                annos,
                options,
                "int_lin_ge_imp",
                constraint_tag,
                pumpkin_constraints::greater_than_or_equals,
            )?,
            "int_lin_gt_imp" => compile_int_lin_imp_predicate(
                context,
                exprs,
                annos,
                options,
                "int_lin_gt_imp",
                constraint_tag,
                pumpkin_constraints::greater_than,
            )?,
            "int_lin_le_imp" => compile_int_lin_imp_predicate(
                context,
                exprs,
                annos,
                options,
                "int_lin_le_imp",
                constraint_tag,
                pumpkin_constraints::less_than_or_equals,
            )?,
            "int_lin_lt_imp" => compile_int_lin_imp_predicate(
                context,
                exprs,
                annos,
                options,
                "int_lin_lt_imp",
                constraint_tag,
                pumpkin_constraints::less_than,
            )?,
            "int_lin_ne_imp" => compile_int_lin_imp_predicate(
                context,
                exprs,
                annos,
                options,
                "int_lin_ne_imp",
                constraint_tag,
                pumpkin_constraints::not_equals,
            )?,

            "int_lin_ne" => compile_int_lin_predicate(
                context,
                exprs,
                annos,
                options,
                "int_lin_ne",
                constraint_tag,
                pumpkin_constraints::not_equals,
            )?,
            "int_lin_ne_reif" => compile_reified_int_lin_predicate(
                context,
                exprs,
                annos,
                options,
                "int_lin_ne_reif",
                constraint_tag,
                pumpkin_constraints::not_equals,
            )?,
            "int_lin_le" => compile_int_lin_predicate(
                context,
                exprs,
                annos,
                options,
                "int_lin_le",
                constraint_tag,
                pumpkin_constraints::less_than_or_equals,
            )?,
            "int_lin_le_reif" => compile_reified_int_lin_predicate(
                context,
                exprs,
                annos,
                options,
                "int_lin_le_reif",
                constraint_tag,
                pumpkin_constraints::less_than_or_equals,
            )?,
            "int_lin_eq" => compile_int_lin_predicate(
                context,
                exprs,
                annos,
                options,
                "int_lin_eq",
                constraint_tag,
                pumpkin_constraints::equals,
            )?,
            "int_lin_eq_reif" => compile_reified_int_lin_predicate(
                context,
                exprs,
                annos,
                options,
                "int_lin_eq_reif",
                constraint_tag,
                pumpkin_constraints::equals,
            )?,
            "int_ne" => compile_binary_int_predicate_with_options(
                context,
                exprs,
                annos,
                options,
                "int_ne",
                constraint_tag,
                pumpkin_constraints::binary_not_equals,
            )?,
            "int_ne_reif" => compile_reified_binary_int_predicate(
                context,
                exprs,
                annos,
                options,
                "int_ne_reif",
                constraint_tag,
                pumpkin_constraints::binary_not_equals,
            )?,
            "int_eq" => compile_binary_int_predicate_with_options(
                context,
                exprs,
                annos,
                options,
                "int_eq",
                constraint_tag,
                pumpkin_constraints::binary_equals,
            )?,
            "int_eq_reif" => compile_reified_binary_int_predicate(
                context,
                exprs,
                annos,
                options,
                "int_eq_reif",
                constraint_tag,
                pumpkin_constraints::binary_equals,
            )?,
            "int_le" => compile_binary_int_predicate_with_options(
                context,
                exprs,
                annos,
                options,
                "int_le",
                constraint_tag,
                pumpkin_constraints::binary_less_than_or_equals,
            )?,
            "int_le_reif" => compile_reified_binary_int_predicate(
                context,
                exprs,
                annos,
                options,
                "int_le_reif",
                constraint_tag,
                pumpkin_constraints::binary_less_than_or_equals,
            )?,
            "int_lt" => compile_binary_int_predicate_with_options(
                context,
                exprs,
                annos,
                options,
                "int_lt",
                constraint_tag,
                pumpkin_constraints::binary_less_than,
            )?,
            "int_lt_reif" => compile_reified_binary_int_predicate(
                context,
                exprs,
                annos,
                options,
                "int_lt_reif",
                constraint_tag,
                pumpkin_constraints::binary_less_than,
            )?,

            "int_plus" => compile_ternary_int_predicate_with_options(
                context,
                exprs,
                annos,
                options,
                "int_plus",
                constraint_tag,
                pumpkin_constraints::plus,
            )?,

            "int_times" => compile_ternary_int_predicate(
                context,
                exprs,
                annos,
                "int_times",
                constraint_tag,
                pumpkin_constraints::times,
            )?,
            "int_div" => compile_ternary_int_predicate(
                context,
                exprs,
                annos,
                "int_div",
                constraint_tag,
                pumpkin_constraints::division,
            )?,
            "int_abs" => compile_binary_int_predicate(
                context,
                exprs,
                annos,
                "int_abs",
                constraint_tag,
                pumpkin_constraints::absolute,
            )?,

            "pumpkin_all_different" => {
                compile_all_different(context, exprs, annos, constraint_tag, options)?
            }
            "pumpkin_circuit" => compile_circuit(context, exprs, annos, constraint_tag, options)?,
            "pumpkin_table_int" => compile_table(context, exprs, annos, constraint_tag)?,
            "pumpkin_table_int_reif" => compile_table_reif(context, exprs, annos, constraint_tag)?,

            "array_bool_and" => compile_array_bool_and(context, exprs, constraint_tag)?,
            "array_bool_element" => compile_array_var_bool_element(
                context,
                exprs,
                "array_bool_element",
                constraint_tag,
            )?,
            "array_var_bool_element" => compile_array_var_bool_element(
                context,
                exprs,
                "array_var_bool_element",
                constraint_tag,
            )?,
            "array_bool_or" => compile_bool_or(context, exprs, constraint_tag)?,
            "pumpkin_bool_xor" => compile_bool_xor(context, exprs, constraint_tag)?,
            "pumpkin_bool_xor_reif" => compile_bool_xor_reif(context, exprs, constraint_tag)?,

            "bool2int" => compile_bool2int(context, exprs, constraint_tag, options)?,

            "bool_lin_eq" => {
                compile_bool_lin_eq_predicate(context, exprs, constraint_tag, options)?
            }

            "bool_lin_le" => {
                compile_bool_lin_le_predicate(context, exprs, constraint_tag, options)?
            }

            "bool_and" => compile_bool_and(context, exprs, constraint_tag)?,
            "bool_clause" => compile_bool_clause(context, exprs, constraint_tag)?,
            "bool_eq" => compile_bool_eq(context, exprs, constraint_tag, options)?,
            "bool_eq_reif" => compile_bool_eq_reif(context, exprs, constraint_tag, options)?,
            "bool_not" => compile_bool_not(context, exprs, constraint_tag, options)?,
            "set_in_reif" => compile_set_in_reif(context, exprs, constraint_tag)?,
            "set_in" => {
                // 'set_in' constraints are handled in pre-processing steps.
                // TODO: remove it from the AST, so it does not need to be matched here
                true
            }

            "pumpkin_cumulative" => compile_cumulative(context, exprs, constraint_tag, options)?,
            "pumpkin_cumulative_var" => todo!(
                "The `cumulative` constraint with variable duration/resource consumption/bound is not implemented yet!"
            ),
            unknown => todo!("unsupported constraint {unknown}"),
        };

        if !is_satisfiable {
            break;
        }
    }

    Ok(())
}

macro_rules! check_parameters {
    ($exprs:ident, $num_parameters:expr, $name:expr) => {
        if $exprs.len() != $num_parameters {
            return Err(FlatZincError::IncorrectNumberOfArguments {
                constraint_id: $name.into(),
                expected: $num_parameters,
                actual: $exprs.len(),
            });
        }
    };
}

fn compile_disjunctive_strict(
    context: &mut CompilationContext<'_>,
    exprs: &[flatzinc::Expr],
    constraint_tag: ConstraintTag,
) -> Result<bool, FlatZincError> {
    check_parameters!(exprs, 2, "pumpkin_cumulative");

    let start_times = context.resolve_integer_variable_array(&exprs[0])?;
    let durations = context.resolve_array_integer_constants(&exprs[1])?;

    assert_eq!(start_times.len(), durations.len());

    let post_result = pumpkin_constraints::disjunctive_strict(
        start_times
            .iter()
            .zip(durations.iter())
            .map(|(&start_time, &duration)| ArgDisjunctiveTask {
                start_time,
                processing_time: duration,
            }),
        constraint_tag,
    )
    .post(context.solver);
    Ok(post_result.is_ok())
}

fn compile_cumulative(
    context: &mut CompilationContext<'_>,
    exprs: &[flatzinc::Expr],
    constraint_tag: ConstraintTag,
    options: &FlatZincOptions,
) -> Result<bool, FlatZincError> {
    check_parameters!(exprs, 4, "pumpkin_cumulative");

    let start_times = context.resolve_integer_variable_array(&exprs[0])?;
    let durations = context.resolve_array_integer_constants(&exprs[1])?;
    let resource_requirements = context.resolve_array_integer_constants(&exprs[2])?;
    let resource_capacity = context.resolve_integer_constant_from_expr(&exprs[3])?;

    assert_eq!(start_times.len(), durations.len());
    assert_eq!(durations.len(), resource_requirements.len());

    let post_result = pumpkin_constraints::cumulative(
        start_times
            .iter()
            .zip(&*durations)
            .zip(&*resource_requirements)
            .map(|((start_time, duration), resource_requirement)| Task {
                start_time: *start_time,
                duration: (*duration)
                    .try_into()
                    .expect("Expected duration to be unsigned"),
                resource_usage: (*resource_requirement)
                    .try_into()
                    .expect("Expected resource usage to be unsigned"),
            }),
        resource_capacity,
        constraint_tag,
        options.cumulative_conflict_only,
    )
    .post(context.solver);
    Ok(post_result.is_ok())
}

fn compile_array_int_maximum(
    context: &mut CompilationContext<'_>,
    exprs: &[flatzinc::Expr],
    constraint_tag: ConstraintTag,
) -> Result<bool, FlatZincError> {
    check_parameters!(exprs, 2, "array_int_maximum");

    let rhs = context.resolve_integer_variable(&exprs[0])?;
    let array = context.resolve_integer_variable_array(&exprs[1])?;

    Ok(
        pumpkin_constraints::maximum(array.as_ref().to_owned(), rhs, constraint_tag)
            .post(context.solver)
            .is_ok(),
    )
}

fn compile_array_int_minimum(
    context: &mut CompilationContext<'_>,
    exprs: &[flatzinc::Expr],
    constraint_tag: ConstraintTag,
) -> Result<bool, FlatZincError> {
    check_parameters!(exprs, 2, "array_int_minimum");

    let rhs = context.resolve_integer_variable(&exprs[0])?;
    let array = context.resolve_integer_variable_array(&exprs[1])?;

    Ok(
        pumpkin_constraints::minimum(array.as_ref().to_owned(), rhs, constraint_tag)
            .post(context.solver)
            .is_ok(),
    )
}

fn compile_set_in_reif(
    context: &mut CompilationContext<'_>,
    exprs: &[flatzinc::Expr],
    constraint_tag: ConstraintTag,
) -> Result<bool, FlatZincError> {
    check_parameters!(exprs, 3, "set_in_reif");

    let variable = context.resolve_integer_variable(&exprs[0])?;
    let set = context.resolve_set_constant(&exprs[1])?;
    let reif = context.resolve_bool_variable(&exprs[2])?;

    let success = match set {
        Set::Interval {
            lower_bound,
            upper_bound,
        } => {
            // `reif -> x \in S`
            // Decomposed to `reif -> x >= lb /\ reif -> x <= ub`
            let forward = context
                .solver
                .add_clause(
                    [
                        !reif.get_true_predicate(),
                        predicate![variable >= lower_bound],
                    ],
                    constraint_tag,
                )
                .is_ok()
                && context
                    .solver
                    .add_clause(
                        [
                            !reif.get_true_predicate(),
                            !predicate![variable >= upper_bound + 1],
                        ],
                        constraint_tag,
                    )
                    .is_ok();

            // `!reif -> x \notin S`
            // Decomposed to `!reif -> (x < lb \/ x > ub)`
            let backward = context
                .solver
                .add_clause(
                    [
                        reif.get_true_predicate(),
                        !predicate![variable >= lower_bound],
                        predicate![variable >= upper_bound + 1],
                    ],
                    constraint_tag,
                )
                .is_ok();

            forward && backward
        }

        Set::Sparse { values } => {
            let clause = values
                .iter()
                .map(|&value| {
                    context
                        .solver
                        .new_literal_for_predicate(predicate![variable == value], constraint_tag)
                })
                .collect::<Vec<_>>();

            pumpkin_constraints::clause(clause, constraint_tag)
                .reify(context.solver, reif)
                .is_ok()
        }
    };

    Ok(success)
}

fn compile_array_var_int_element(
    context: &mut CompilationContext<'_>,
    exprs: &[flatzinc::Expr],
    constraint_tag: ConstraintTag,
) -> Result<bool, FlatZincError> {
    check_parameters!(exprs, 3, "array_var_int_element");

    let index = context.resolve_integer_variable(&exprs[0])?.offset(-1);
    let array = context.resolve_integer_variable_array(&exprs[1])?;
    let rhs = context.resolve_integer_variable(&exprs[2])?;

    Ok(
        pumpkin_constraints::element(index, array.as_ref().to_owned(), rhs, constraint_tag)
            .post(context.solver)
            .is_ok(),
    )
}

fn compile_bool_not(
    context: &mut CompilationContext<'_>,
    exprs: &[flatzinc::Expr],
    constraint_tag: ConstraintTag,
    options: &FlatZincOptions,
) -> Result<bool, FlatZincError> {
    // TODO: Take this constraint into account when creating variables, as these can be opposite
    // literals of the same PropositionalVariable. Unsure how often this actually appears in models
    // though.

    check_parameters!(exprs, 2, "bool_not");

    let a = context.resolve_bool_variable(&exprs[0])?;
    let b = context.resolve_bool_variable(&exprs[1])?;

    Ok(
        pumpkin_constraints::binary_not_equals(a, b, constraint_tag, options.linear_conflict_only)
            .post(context.solver)
            .is_ok(),
    )
}

fn compile_bool_eq_reif(
    context: &mut CompilationContext<'_>,
    exprs: &[flatzinc::Expr],
    constraint_tag: ConstraintTag,
    options: &FlatZincOptions,
) -> Result<bool, FlatZincError> {
    check_parameters!(exprs, 3, "bool_eq_reif");

    let a = context.resolve_bool_variable(&exprs[0])?;
    let b = context.resolve_bool_variable(&exprs[1])?;
    let r = context.resolve_bool_variable(&exprs[2])?;

    Ok(
        pumpkin_constraints::binary_equals(a, b, constraint_tag, options.linear_conflict_only)
            .reify(context.solver, r)
            .is_ok(),
    )
}

fn compile_bool_eq(
    context: &mut CompilationContext<'_>,
    exprs: &[flatzinc::Expr],
    constraint_tag: ConstraintTag,
    options: &FlatZincOptions,
) -> Result<bool, FlatZincError> {
    // TODO: Take this constraint into account when merging equivalence classes. Unsure how often
    // this actually appears in models though.
    check_parameters!(exprs, 2, "bool_eq");

    let a = context.resolve_bool_variable(&exprs[0])?;
    let b = context.resolve_bool_variable(&exprs[1])?;

    Ok(
        pumpkin_constraints::binary_equals(a, b, constraint_tag, options.linear_conflict_only)
            .post(context.solver)
            .is_ok(),
    )
}

fn compile_bool_clause(
    context: &mut CompilationContext<'_>,
    exprs: &[flatzinc::Expr],
    constraint_tag: ConstraintTag,
) -> Result<bool, FlatZincError> {
    check_parameters!(exprs, 2, "bool_clause");

    let clause_1 = context.resolve_bool_variable_array(&exprs[0])?;
    let clause_2 = context.resolve_bool_variable_array(&exprs[1])?;

    let clause: Vec<Predicate> = clause_1
        .iter()
        .cloned()
        .chain(clause_2.iter().map(|literal| !*literal))
        .map(|literal| literal.get_true_predicate())
        .collect();

    Ok(context.solver.add_clause(clause, constraint_tag).is_ok())
}

fn compile_bool_and(
    context: &mut CompilationContext<'_>,
    exprs: &[flatzinc::Expr],
    constraint_tag: ConstraintTag,
) -> Result<bool, FlatZincError> {
    check_parameters!(exprs, 2, "bool_and");

    let a = context.resolve_bool_variable(&exprs[0])?;
    let b = context.resolve_bool_variable(&exprs[1])?;
    let r = context.resolve_bool_variable(&exprs[2])?;

    Ok(pumpkin_constraints::conjunction([a, b], constraint_tag)
        .reify(context.solver, r)
        .is_ok())
}

fn compile_bool2int(
    context: &mut CompilationContext<'_>,
    exprs: &[flatzinc::Expr],
    constraint_tag: ConstraintTag,
    options: &FlatZincOptions,
) -> Result<bool, FlatZincError> {
    // TODO: Perhaps we want to add a phase in the compiler that directly uses the literal
    // corresponding to the predicate [b = 1] for the boolean parameter in this constraint.
    // See https://emir-demirovic.atlassian.net/browse/PUM-89

    check_parameters!(exprs, 2, "bool2int");

    let a = context.resolve_bool_variable(&exprs[0])?;
    let b = context.resolve_integer_variable(&exprs[1])?;

    Ok(pumpkin_constraints::binary_equals(
        a.get_integer_variable(),
        b.scaled(1),
        constraint_tag,
        options.linear_conflict_only,
    )
    .post(context.solver)
    .is_ok())
}

fn compile_bool_or(
    context: &mut CompilationContext<'_>,
    exprs: &[flatzinc::Expr],
    constraint_tag: ConstraintTag,
) -> Result<bool, FlatZincError> {
    check_parameters!(exprs, 2, "bool_or");

    let clause = context.resolve_bool_variable_array(&exprs[0])?;
    let r = context.resolve_bool_variable(&exprs[1])?;

    Ok(pumpkin_constraints::clause(clause.as_ref(), constraint_tag)
        .reify(context.solver, r)
        .is_ok())
}

fn compile_bool_xor(
    context: &mut CompilationContext<'_>,
    exprs: &[flatzinc::Expr],
    constraint_tag: ConstraintTag,
) -> Result<bool, FlatZincError> {
    check_parameters!(exprs, 2, "pumpkin_bool_xor");

    let a = context
        .resolve_bool_variable(&exprs[0])?
        .get_true_predicate();
    let b = context
        .resolve_bool_variable(&exprs[1])?
        .get_true_predicate();

    let c1 = context.solver.add_clause([!a, !b], constraint_tag).is_ok();
    let c2 = context.solver.add_clause([b, a], constraint_tag).is_ok();

    Ok(c1 && c2)
}

fn compile_bool_xor_reif(
    context: &mut CompilationContext<'_>,
    exprs: &[flatzinc::Expr],
    constraint_tag: ConstraintTag,
) -> Result<bool, FlatZincError> {
    check_parameters!(exprs, 3, "pumpkin_bool_xor_reif");

    let a = context.resolve_bool_variable(&exprs[0])?;
    let b = context.resolve_bool_variable(&exprs[1])?;
    let r = context.resolve_bool_variable(&exprs[2])?;

    let c1 = pumpkin_constraints::clause([!a, !b, !r], constraint_tag)
        .post(context.solver)
        .is_ok();
    let c2 = pumpkin_constraints::clause([!a, b, r], constraint_tag)
        .post(context.solver)
        .is_ok();
    let c3 = pumpkin_constraints::clause([a, !b, r], constraint_tag)
        .post(context.solver)
        .is_ok();
    let c4 = pumpkin_constraints::clause([a, b, !r], constraint_tag)
        .post(context.solver)
        .is_ok();

    Ok(c1 && c2 && c3 && c4)
}

fn compile_array_var_bool_element(
    context: &mut CompilationContext<'_>,
    exprs: &[flatzinc::Expr],
    name: &str,
    constraint_tag: ConstraintTag,
) -> Result<bool, FlatZincError> {
    check_parameters!(exprs, 3, name);

    let index = context.resolve_integer_variable(&exprs[0])?.offset(-1);
    let array = context.resolve_bool_variable_array(&exprs[1])?;
    let rhs = context.resolve_bool_variable(&exprs[2])?;

    Ok(
        pumpkin_constraints::element(index, array.iter().cloned(), rhs, constraint_tag)
            .post(context.solver)
            .is_ok(),
    )
}

fn compile_array_bool_and(
    context: &mut CompilationContext<'_>,
    exprs: &[flatzinc::Expr],
    constraint_tag: ConstraintTag,
) -> Result<bool, FlatZincError> {
    check_parameters!(exprs, 2, "array_bool_and");

    let conjunction = context.resolve_bool_variable_array(&exprs[0])?;
    let r = context.resolve_bool_variable(&exprs[1])?;

    Ok(
        pumpkin_constraints::conjunction(conjunction.as_ref(), constraint_tag)
            .reify(context.solver, r)
            .is_ok(),
    )
}

fn compile_ternary_int_predicate_with_options<C: Constraint>(
    context: &mut CompilationContext,
    exprs: &[flatzinc::Expr],
    _: &[flatzinc::Annotation],
    options: &FlatZincOptions,
    predicate_name: &str,
    constraint_tag: ConstraintTag,
    create_constraint: impl FnOnce(DomainId, DomainId, DomainId, ConstraintTag, bool) -> C,
) -> Result<bool, FlatZincError> {
    check_parameters!(exprs, 3, predicate_name);

    let a = context.resolve_integer_variable(&exprs[0])?;
    let b = context.resolve_integer_variable(&exprs[1])?;
    let c = context.resolve_integer_variable(&exprs[2])?;

    let constraint = create_constraint(a, b, c, constraint_tag, options.linear_conflict_only);
    Ok(constraint.post(context.solver).is_ok())
}

fn compile_ternary_int_predicate<C: Constraint>(
    context: &mut CompilationContext,
    exprs: &[flatzinc::Expr],
    _: &[flatzinc::Annotation],
    predicate_name: &str,
    constraint_tag: ConstraintTag,
    create_constraint: impl FnOnce(DomainId, DomainId, DomainId, ConstraintTag) -> C,
) -> Result<bool, FlatZincError> {
    check_parameters!(exprs, 3, predicate_name);

    let a = context.resolve_integer_variable(&exprs[0])?;
    let b = context.resolve_integer_variable(&exprs[1])?;
    let c = context.resolve_integer_variable(&exprs[2])?;

    let constraint = create_constraint(a, b, c, constraint_tag);
    Ok(constraint.post(context.solver).is_ok())
}

fn compile_binary_int_predicate<C: Constraint>(
    context: &mut CompilationContext,
    exprs: &[flatzinc::Expr],
    _: &[flatzinc::Annotation],
    predicate_name: &str,
    constraint_tag: ConstraintTag,
    create_constraint: impl FnOnce(DomainId, DomainId, ConstraintTag) -> C,
) -> Result<bool, FlatZincError> {
    check_parameters!(exprs, 2, predicate_name);

    let a = context.resolve_integer_variable(&exprs[0])?;
    let b = context.resolve_integer_variable(&exprs[1])?;

    let constraint = create_constraint(a, b, constraint_tag);
    Ok(constraint.post(context.solver).is_ok())
}

fn compile_binary_int_predicate_with_options<C: Constraint>(
    context: &mut CompilationContext,
    exprs: &[flatzinc::Expr],
    _: &[flatzinc::Annotation],
    options: &FlatZincOptions,
    predicate_name: &str,
    constraint_tag: ConstraintTag,
    create_constraint: impl FnOnce(DomainId, DomainId, ConstraintTag, bool) -> C,
) -> Result<bool, FlatZincError> {
    check_parameters!(exprs, 2, predicate_name);

    let a = context.resolve_integer_variable(&exprs[0])?;
    let b = context.resolve_integer_variable(&exprs[1])?;

    let constraint = create_constraint(a, b, constraint_tag, options.linear_conflict_only);
    Ok(constraint.post(context.solver).is_ok())
}

fn compile_reified_binary_int_predicate<C: NegatableConstraint>(
    context: &mut CompilationContext,
    exprs: &[flatzinc::Expr],
    _: &[flatzinc::Annotation],
    options: &FlatZincOptions,
    predicate_name: &str,
    constraint_tag: ConstraintTag,
    create_constraint: impl FnOnce(DomainId, DomainId, ConstraintTag, bool) -> C,
) -> Result<bool, FlatZincError> {
    check_parameters!(exprs, 3, predicate_name);

    let a = context.resolve_integer_variable(&exprs[0])?;
    let b = context.resolve_integer_variable(&exprs[1])?;
    let reif = context.resolve_bool_variable(&exprs[2])?;

    let constraint = create_constraint(a, b, constraint_tag, options.linear_conflict_only);
    Ok(constraint.reify(context.solver, reif).is_ok())
}

fn weighted_vars(weights: Rc<[i32]>, vars: Rc<[DomainId]>) -> Box<[AffineView<DomainId>]> {
    vars.iter()
        .zip(weights.iter())
        .filter(|&(_, w)| *w != 0)
        .map(|(x_i, &w_i)| x_i.scaled(w_i))
        .collect::<Box<[_]>>()
}

fn compile_int_lin_predicate<C: Constraint>(
    context: &mut CompilationContext,
    exprs: &[flatzinc::Expr],
    _: &[flatzinc::Annotation],
    options: &FlatZincOptions,
    predicate_name: &str,
    constraint_tag: ConstraintTag,
    create_constraint: impl FnOnce(Box<[AffineView<DomainId>]>, i32, ConstraintTag, bool) -> C,
) -> Result<bool, FlatZincError> {
    check_parameters!(exprs, 3, predicate_name);

    let weights = context.resolve_array_integer_constants(&exprs[0])?;
    let vars = context.resolve_integer_variable_array(&exprs[1])?;
    let rhs = context.resolve_integer_constant_from_expr(&exprs[2])?;

    let terms = weighted_vars(weights, vars);

    let constraint = create_constraint(terms, rhs, constraint_tag, options.linear_conflict_only);
    Ok(constraint.post(context.solver).is_ok())
}

fn compile_reified_int_lin_predicate<C: NegatableConstraint>(
    context: &mut CompilationContext,
    exprs: &[flatzinc::Expr],
    _: &[flatzinc::Annotation],
    options: &FlatZincOptions,
    predicate_name: &str,
    constraint_tag: ConstraintTag,
    create_constraint: impl FnOnce(Box<[AffineView<DomainId>]>, i32, ConstraintTag, bool) -> C,
) -> Result<bool, FlatZincError> {
    check_parameters!(exprs, 4, predicate_name);

    let weights = context.resolve_array_integer_constants(&exprs[0])?;
    let vars = context.resolve_integer_variable_array(&exprs[1])?;
    let rhs = context.resolve_integer_constant_from_expr(&exprs[2])?;
    let reif = context.resolve_bool_variable(&exprs[3])?;

    let terms = weighted_vars(weights, vars);

    let constraint = create_constraint(terms, rhs, constraint_tag, options.linear_conflict_only);
    Ok(constraint.reify(context.solver, reif).is_ok())
}

fn compile_int_lin_imp_predicate<C: Constraint>(
    context: &mut CompilationContext,
    exprs: &[flatzinc::Expr],
    _: &[flatzinc::Annotation],
    options: &FlatZincOptions,
    predicate_name: &str,
    constraint_tag: ConstraintTag,
    create_constraint: impl FnOnce(Box<[AffineView<DomainId>]>, i32, ConstraintTag, bool) -> C,
) -> Result<bool, FlatZincError> {
    check_parameters!(exprs, 4, predicate_name);

    let weights = context.resolve_array_integer_constants(&exprs[0])?;
    let vars = context.resolve_integer_variable_array(&exprs[1])?;
    let rhs = context.resolve_integer_constant_from_expr(&exprs[2])?;
    let reif = context.resolve_bool_variable(&exprs[3])?;

    let terms = weighted_vars(weights, vars);

    let constraint = create_constraint(terms, rhs, constraint_tag, options.linear_conflict_only);
    Ok(constraint.implied_by(context.solver, reif).is_ok())
}

fn compile_binary_int_imp<C: Constraint>(
    context: &mut CompilationContext,
    exprs: &[flatzinc::Expr],
    _: &[flatzinc::Annotation],
    options: &FlatZincOptions,
    predicate_name: &str,
    constraint_tag: ConstraintTag,
    create_constraint: impl FnOnce(DomainId, DomainId, ConstraintTag, bool) -> C,
) -> Result<bool, FlatZincError> {
    check_parameters!(exprs, 3, predicate_name);

    let a = context.resolve_integer_variable(&exprs[0])?;
    let b = context.resolve_integer_variable(&exprs[1])?;
    let reif = context.resolve_bool_variable(&exprs[2])?;

    let constraint = create_constraint(a, b, constraint_tag, options.linear_conflict_only);
    Ok(constraint.implied_by(context.solver, reif).is_ok())
}

fn compile_bool_lin_eq_predicate(
    context: &mut CompilationContext,
    exprs: &[flatzinc::Expr],
    constraint_tag: ConstraintTag,
    options: &FlatZincOptions,
) -> Result<bool, FlatZincError> {
    check_parameters!(exprs, 3, "bool_lin_eq");

    let weights = context.resolve_array_integer_constants(&exprs[0])?;
    let bools = context.resolve_bool_variable_array(&exprs[1])?;
    let rhs = context.resolve_integer_variable(&exprs[2])?;

    Ok(pumpkin_constraints::boolean_equals(
        weights.as_ref().to_owned(),
        bools.as_ref().to_owned(),
        rhs,
        constraint_tag,
        options.linear_conflict_only,
    )
    .post(context.solver)
    .is_ok())
}

fn compile_bool_lin_le_predicate(
    context: &mut CompilationContext,
    exprs: &[flatzinc::Expr],
    constraint_tag: ConstraintTag,
    options: &FlatZincOptions,
) -> Result<bool, FlatZincError> {
    check_parameters!(exprs, 3, "bool_lin_le");

    let weights = context.resolve_array_integer_constants(&exprs[0])?;
    let bools = context.resolve_bool_variable_array(&exprs[1])?;
    let rhs = context.resolve_integer_constant_from_expr(&exprs[2])?;

    Ok(pumpkin_constraints::boolean_less_than_or_equals(
        weights.as_ref().to_owned(),
        bools.as_ref().to_owned(),
        rhs,
        constraint_tag,
        options.linear_conflict_only,
    )
    .post(context.solver)
    .is_ok())
}

fn compile_all_different(
    context: &mut CompilationContext,
    exprs: &[flatzinc::Expr],
    _: &[flatzinc::Annotation],
    constraint_tag: ConstraintTag,
    options: &FlatZincOptions,
) -> Result<bool, FlatZincError> {
    check_parameters!(exprs, 1, "fzn_all_different");

    let variables = context.resolve_integer_variable_array(&exprs[0])?.to_vec();
    Ok(pumpkin_constraints::all_different(
        variables,
        constraint_tag,
        options.all_different_conflict_only,
    )
    .post(context.solver)
    .is_ok())
}

fn compile_circuit(
    context: &mut CompilationContext,
    exprs: &[flatzinc::Expr],
    _: &[flatzinc::Annotation],
    constraint_tag: ConstraintTag,
    options: &FlatZincOptions,
) -> Result<bool, FlatZincError> {
    check_parameters!(exprs, 1, "fzn_circuit");

    let variables = context.resolve_integer_variable_array(&exprs[0])?.to_vec();
    Ok(
        pumpkin_constraints::circuit(variables, constraint_tag, options.circuit_conflict_only)
            .post(context.solver)
            .is_ok(),
    )
}

fn compile_table(
    context: &mut CompilationContext,
    exprs: &[flatzinc::Expr],
    _: &[flatzinc::Annotation],
    constraint_tag: ConstraintTag,
) -> Result<bool, FlatZincError> {
    check_parameters!(exprs, 2, "pumpkin_table_int");

    let variables = context.resolve_integer_variable_array(&exprs[0])?.to_vec();

    let flat_table = context.resolve_array_integer_constants(&exprs[1])?;
    let table = create_table(flat_table, variables.len());

    Ok(pumpkin_constraints::table(variables, table, constraint_tag)
        .post(context.solver)
        .is_ok())
}

fn compile_table_reif(
    context: &mut CompilationContext,
    exprs: &[flatzinc::Expr],
    _: &[flatzinc::Annotation],
    constraint_tag: ConstraintTag,
) -> Result<bool, FlatZincError> {
    check_parameters!(exprs, 3, "pumpkin_table_int_reif");

    let variables = context.resolve_integer_variable_array(&exprs[0])?.to_vec();

    let flat_table = context.resolve_array_integer_constants(&exprs[1])?;
    let table = create_table(flat_table, variables.len());

    let reified = context.resolve_bool_variable(&exprs[2])?;

    Ok(pumpkin_constraints::table(variables, table, constraint_tag)
        .reify(context.solver, reified)
        .is_ok())
}

fn create_table(flat_table: Rc<[i32]>, num_variables: usize) -> Vec<Vec<i32>> {
    let table = flat_table
        .iter()
        .copied()
        .fold(vec![], |mut acc, next_value| {
            if acc
                .last()
                .map(|row: &Vec<i32>| row.len() == num_variables)
                .unwrap_or(true)
            {
                acc.push(vec![]);
            }

            acc.last_mut().unwrap().push(next_value);

            acc
        });

    if !flat_table.is_empty() {
        assert_eq!(num_variables, table.last().unwrap().len());
    }

    table
}
