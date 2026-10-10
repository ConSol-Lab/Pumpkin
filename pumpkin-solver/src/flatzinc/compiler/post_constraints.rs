//! Compile constraints into CP propagators

use std::rc::Rc;

use pumpkin_propagators::disjunctive::ArgDisjunctiveTask;
use pumpkin_solver::core::constraints::Constraint;
use pumpkin_solver::core::constraints::NegatableConstraint;
use pumpkin_solver::core::predicate;
use pumpkin_solver::core::predicates::Predicate;
use pumpkin_solver::core::proof::ConstraintTag;
use pumpkin_solver::core::variables::AffineView;
use pumpkin_solver::core::variables::DomainId;
use pumpkin_solver::core::variables::TransformableVariable;

use super::context::CompilationContext;
use crate::flatzinc::FlatZincError;
use crate::flatzinc::FlatZincOptions;
use crate::flatzinc::ProofFormat;
use crate::flatzinc::ast::FlatZincAst;
use crate::flatzinc::compiler::context::Set;
use crate::flatzinc::preamble::Decomposition;
use crate::flatzinc::preamble::DecompositionKind;

pub(crate) fn run(
    _: &FlatZincAst,
    context: &mut CompilationContext,
    options: &FlatZincOptions,
) -> Result<(), FlatZincError> {
    // 'set_in' constraints are handled in pre-processing steps, which restrict the domain of the
    // variable to the set. fzn-croc-check holds the constraint apart from the declared domains,
    // so the proof cites it for what the set excludes. The sets are named before any constraint
    // is posted, since posting one can explain a predicate a set implies at the root.
    if context.proof_format == ProofFormat::Croc && context.solver.is_logging_proof() {
        let set_ins: Vec<(ConstraintTag, Vec<flatzinc::Expr>)> = context
            .constraints
            .iter()
            .filter(|(_, item)| item.id == "set_in")
            .map(|(tag, item)| (*tag, item.exprs.clone()))
            .collect();
        for (constraint_tag, exprs) in set_ins {
            name_set_in_domain(context, &exprs, constraint_tag)?;
        }
    }
    for (constraint_tag, constraint_item) in std::mem::take(&mut context.constraints) {
        let flatzinc::ConstraintItem { id, exprs, annos } = &constraint_item;
        // fzn-croc-check reads these as a reified or half-reified constraint, whose inferences
        // cite the rule of the inner constraint wrapped as reified(R) or half_reified(R).
        if context.proof_format == ProofFormat::Croc
            && let Some(fully) = reification_of(id)
        {
            context.solver.name_reification(constraint_tag, fully);
        }

        // Constraints Pumpkin decomposes are cited under the rule for the whole constraint.
        match id.as_str() {
            "pumpkin_all_different" | "all_different_int" | "fzn_all_different_int" => context
                .solver
                .name_rule_of_constraint(constraint_tag, "all_different_hall"),
            "array_int_minimum" | "int_min" => context
                .solver
                .name_rule_of_constraint(constraint_tag, "minimum"),
            "bool_clause" => context
                .solver
                .name_rule_of_constraint(constraint_tag, "clause"),
            "array_bool_and" | "bool_and" => context
                .solver
                .name_rule_of_constraint(constraint_tag, "boolean_and"),
            "array_bool_or" | "bool_or" => context
                .solver
                .name_rule_of_constraint(constraint_tag, "boolean_or"),
            // fzn-croc-check reads set_in_reif as set_in under an indicator.
            "set_in_reif" if context.proof_format == ProofFormat::Croc => context
                .solver
                .name_rule_of_constraint(constraint_tag, "set_in"),
            // fzn-croc-check reads bool_not(a, b) as the equality a + b = 1.
            "bool_not" if context.proof_format == ProofFormat::Croc => context
                .solver
                .name_rule_of_constraint(constraint_tag, "linear_equal_binary"),
            _ => {}
        }

        match id.as_str() {
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
                "int_eq_imp",
                constraint_tag,
                pumpkin_constraints::binary_equals,
            )?,
            "int_ge_imp" => compile_binary_int_imp(
                context,
                exprs,
                annos,
                "int_ge_imp",
                constraint_tag,
                pumpkin_constraints::binary_greater_than_or_equals,
            )?,
            "int_gt_imp" => compile_binary_int_imp(
                context,
                exprs,
                annos,
                "int_gt_imp",
                constraint_tag,
                pumpkin_constraints::binary_greater_than,
            )?,
            "int_le_imp" => compile_binary_int_imp(
                context,
                exprs,
                annos,
                "int_le_imp",
                constraint_tag,
                pumpkin_constraints::binary_less_than_or_equals,
            )?,
            "int_lt_imp" => compile_binary_int_imp(
                context,
                exprs,
                annos,
                "int_lt_imp",
                constraint_tag,
                pumpkin_constraints::binary_less_than,
            )?,
            "int_ne_imp" => compile_binary_int_imp(
                context,
                exprs,
                annos,
                "int_ne_imp",
                constraint_tag,
                pumpkin_constraints::binary_not_equals,
            )?,

            "int_lin_eq_imp" => compile_int_lin_imp_predicate(
                context,
                exprs,
                annos,
                "int_lin_eq_imp",
                constraint_tag,
                pumpkin_constraints::equals,
            )?,
            "int_lin_ge_imp" => compile_int_lin_imp_predicate(
                context,
                exprs,
                annos,
                "int_lin_ge_imp",
                constraint_tag,
                pumpkin_constraints::greater_than_or_equals,
            )?,
            "int_lin_gt_imp" => compile_int_lin_imp_predicate(
                context,
                exprs,
                annos,
                "int_lin_gt_imp",
                constraint_tag,
                pumpkin_constraints::greater_than,
            )?,
            "int_lin_le_imp" => compile_int_lin_imp_predicate(
                context,
                exprs,
                annos,
                "int_lin_le_imp",
                constraint_tag,
                pumpkin_constraints::less_than_or_equals,
            )?,
            "int_lin_lt_imp" => compile_int_lin_imp_predicate(
                context,
                exprs,
                annos,
                "int_lin_lt_imp",
                constraint_tag,
                pumpkin_constraints::less_than,
            )?,
            "int_lin_ne_imp" => compile_int_lin_imp_predicate(
                context,
                exprs,
                annos,
                "int_lin_ne_imp",
                constraint_tag,
                pumpkin_constraints::not_equals,
            )?,

            "int_lin_ne" => compile_int_lin_predicate(
                context,
                exprs,
                annos,
                "int_lin_ne",
                constraint_tag,
                pumpkin_constraints::not_equals,
            )?,
            "int_lin_ne_reif" => compile_reified_int_lin_predicate(
                context,
                exprs,
                annos,
                "int_lin_ne_reif",
                constraint_tag,
                pumpkin_constraints::not_equals,
            )?,
            "int_lin_le" => compile_int_lin_predicate(
                context,
                exprs,
                annos,
                "int_lin_le",
                constraint_tag,
                pumpkin_constraints::less_than_or_equals,
            )?,
            "int_lin_le_reif" => compile_reified_int_lin_predicate(
                context,
                exprs,
                annos,
                "int_lin_le_reif",
                constraint_tag,
                pumpkin_constraints::less_than_or_equals,
            )?,
            "int_lin_eq" => compile_int_lin_predicate(
                context,
                exprs,
                annos,
                "int_lin_eq",
                constraint_tag,
                pumpkin_constraints::equals,
            )?,
            "int_lin_eq_reif" => compile_reified_int_lin_predicate(
                context,
                exprs,
                annos,
                "int_lin_eq_reif",
                constraint_tag,
                pumpkin_constraints::equals,
            )?,
            "int_ne" => compile_binary_int_predicate(
                context,
                exprs,
                annos,
                "int_ne",
                constraint_tag,
                pumpkin_constraints::binary_not_equals,
            )?,
            "int_ne_reif" => compile_reified_binary_int_predicate(
                context,
                exprs,
                annos,
                "int_ne_reif",
                constraint_tag,
                pumpkin_constraints::binary_not_equals,
            )?,
            "int_eq" => compile_binary_int_predicate(
                context,
                exprs,
                annos,
                "int_eq",
                constraint_tag,
                pumpkin_constraints::binary_equals,
            )?,
            "int_eq_reif" => compile_reified_binary_int_predicate(
                context,
                exprs,
                annos,
                "int_eq_reif",
                constraint_tag,
                pumpkin_constraints::binary_equals,
            )?,
            "int_le" => compile_binary_int_predicate(
                context,
                exprs,
                annos,
                "int_le",
                constraint_tag,
                pumpkin_constraints::binary_less_than_or_equals,
            )?,
            "int_le_reif" => compile_reified_binary_int_predicate(
                context,
                exprs,
                annos,
                "int_le_reif",
                constraint_tag,
                pumpkin_constraints::binary_less_than_or_equals,
            )?,
            "int_lt" => compile_binary_int_predicate(
                context,
                exprs,
                annos,
                "int_lt",
                constraint_tag,
                pumpkin_constraints::binary_less_than,
            )?,
            "int_lt_reif" => compile_reified_binary_int_predicate(
                context,
                exprs,
                annos,
                "int_lt_reif",
                constraint_tag,
                pumpkin_constraints::binary_less_than,
            )?,

            "int_plus" => compile_ternary_int_predicate(
                context,
                exprs,
                annos,
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
                compile_all_different(context, exprs, annos, constraint_tag)?
            }
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

            "bool2int" => compile_bool2int(context, exprs, constraint_tag)?,

            "bool_lin_eq" => compile_bool_lin_eq_predicate(context, exprs, constraint_tag)?,

            "bool_lin_le" => compile_bool_lin_le_predicate(context, exprs, constraint_tag)?,

            "bool_and" => compile_bool_and(context, exprs, constraint_tag)?,
            "bool_clause" => compile_bool_clause(context, exprs, constraint_tag)?,
            "bool_eq" => compile_bool_eq(context, exprs, constraint_tag)?,
            "bool_eq_reif" => compile_bool_eq_reif(context, exprs, constraint_tag)?,
            "bool_not" => compile_bool_not(context, exprs, constraint_tag)?,
            "set_in_reif" => compile_set_in_reif(context, exprs, constraint_tag)?,
            // Named before the constraints were posted.
            "set_in" => {}

            "pumpkin_cumulative" => compile_cumulative(context, exprs, options, constraint_tag)?,
            "pumpkin_cumulative_var" => todo!(
                "The `cumulative` constraint with variable duration/resource consumption/bound is not implemented yet!"
            ),
            unknown => todo!("unsupported constraint {unknown}"),
        };
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
) -> Result<(), FlatZincError> {
    check_parameters!(exprs, 2, "pumpkin_cumulative");

    let start_times = context.resolve_integer_variable_array(&exprs[0])?;
    let durations = context.resolve_array_integer_constants(&exprs[1])?;

    assert_eq!(start_times.len(), durations.len());

    pumpkin_constraints::disjunctive_strict(
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

    Ok(())
}

fn compile_cumulative(
    context: &mut CompilationContext<'_>,
    exprs: &[flatzinc::Expr],
    options: &FlatZincOptions,
    constraint_tag: ConstraintTag,
) -> Result<(), FlatZincError> {
    check_parameters!(exprs, 4, "pumpkin_cumulative");

    let start_times = context.resolve_integer_variable_array(&exprs[0])?;
    let durations = context.resolve_array_integer_constants(&exprs[1])?;
    let resource_requirements = context.resolve_array_integer_constants(&exprs[2])?;
    let resource_capacity = context.resolve_integer_constant_from_expr(&exprs[3])?;

    pumpkin_constraints::cumulative_with_options(
        start_times.iter().copied(),
        durations.iter().copied(),
        resource_requirements.iter().copied(),
        resource_capacity,
        options.cumulative_options,
        constraint_tag,
    )
    .post(context.solver);

    Ok(())
}

fn compile_array_int_maximum(
    context: &mut CompilationContext<'_>,
    exprs: &[flatzinc::Expr],
    constraint_tag: ConstraintTag,
) -> Result<(), FlatZincError> {
    check_parameters!(exprs, 2, "array_int_maximum");

    let rhs = context.resolve_integer_variable(&exprs[0])?;
    let array = context.resolve_integer_variable_array(&exprs[1])?;

    pumpkin_constraints::maximum(array.as_ref().to_owned(), rhs, constraint_tag)
        .post(context.solver);

    Ok(())
}

fn compile_array_int_minimum(
    context: &mut CompilationContext<'_>,
    exprs: &[flatzinc::Expr],
    constraint_tag: ConstraintTag,
) -> Result<(), FlatZincError> {
    check_parameters!(exprs, 2, "array_int_minimum");

    let rhs = context.resolve_integer_variable(&exprs[0])?;
    let array = context.resolve_integer_variable_array(&exprs[1])?;

    pumpkin_constraints::minimum(array.as_ref().to_owned(), rhs, constraint_tag)
        .post(context.solver);

    Ok(())
}

/// Whether fzn-croc-check reads the FlatZinc constraint as reified (`Some(true)`) or half-reified
/// (`Some(false)`), or as neither.
fn reification_of(id: &str) -> Option<bool> {
    match id {
        "int_lin_le_reif"
        | "int_lin_eq_reif"
        | "int_lin_ne_reif"
        | "int_le_reif"
        | "int_lt_reif"
        | "int_eq_reif"
        | "bool_eq_reif"
        | "int_ne_reif"
        | "pumpkin_bool_xor_reif"
        | "set_in_reif" => Some(true),
        "int_lin_le_imp" | "int_lin_eq_imp" | "int_lin_ne_imp" | "int_le_imp" | "int_lt_imp"
        | "int_eq_imp" | "int_ne_imp" | "set_in_imp" => Some(false),
        _ => None,
    }
}

/// Give the proof the set of a `set_in` constraint over a variable, as ranges of values, so that
/// a bound the set implies at the root cites the constraint.
fn name_set_in_domain(
    context: &mut CompilationContext<'_>,
    exprs: &[flatzinc::Expr],
    constraint_tag: ConstraintTag,
) -> Result<(), FlatZincError> {
    check_parameters!(exprs, 2, "set_in");
    let Ok(member) = context.resolve_integer_variable(&exprs[0]) else {
        return Ok(());
    };
    let ranges = match context.resolve_set_constant(&exprs[1])? {
        Set::Interval {
            lower_bound,
            upper_bound,
        } => vec![(lower_bound, upper_bound)],
        Set::Sparse { values } => {
            let mut values = values.to_vec();
            values.sort_unstable();
            values.dedup();
            let mut ranges: Vec<(i32, i32)> = vec![];
            for value in values {
                match ranges.last_mut() {
                    Some((_, upper)) if *upper + 1 == value => *upper = value,
                    _ => ranges.push((value, value)),
                }
            }
            ranges
        }
    };
    context
        .solver
        .name_set_in_domain(constraint_tag, member, ranges);
    Ok(())
}

fn compile_set_in_reif(
    context: &mut CompilationContext<'_>,
    exprs: &[flatzinc::Expr],
    constraint_tag: ConstraintTag,
) -> Result<(), FlatZincError> {
    check_parameters!(exprs, 3, "set_in_reif");

    let variable = context.resolve_integer_variable(&exprs[0])?;
    let set = context.resolve_set_constant(&exprs[1])?;
    let reif = context.resolve_bool_variable(&exprs[2])?;

    match set {
        Set::Interval {
            lower_bound,
            upper_bound,
        } => {
            // `reif -> x \in S`
            // Decomposed to `reif -> x >= lb /\ reif -> x <= ub`
            context.solver.add_clause(
                [
                    !reif.get_true_predicate(),
                    predicate![variable >= lower_bound],
                ],
                constraint_tag,
            );
            context.solver.add_clause(
                [
                    !reif.get_true_predicate(),
                    !predicate![variable >= upper_bound + 1],
                ],
                constraint_tag,
            );

            // `!reif -> x \notin S`
            // Decomposed to `!reif -> (x < lb \/ x > ub)`
            context.solver.add_clause(
                [
                    reif.get_true_predicate(),
                    !predicate![variable >= lower_bound],
                    predicate![variable >= upper_bound + 1],
                ],
                constraint_tag,
            );
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

            pumpkin_constraints::clause(clause, constraint_tag).reify(context.solver, reif)
        }
    }

    Ok(())
}

fn compile_array_var_int_element(
    context: &mut CompilationContext<'_>,
    exprs: &[flatzinc::Expr],
    constraint_tag: ConstraintTag,
) -> Result<(), FlatZincError> {
    check_parameters!(exprs, 3, "array_var_int_element");

    let index = context.resolve_integer_variable(&exprs[0])?.offset(-1);
    let array = context.resolve_integer_variable_array(&exprs[1])?;
    let rhs = context.resolve_integer_variable(&exprs[2])?;

    pumpkin_constraints::element(index, array.as_ref().to_owned(), rhs, constraint_tag)
        .post(context.solver);
    Ok(())
}

fn compile_bool_not(
    context: &mut CompilationContext<'_>,
    exprs: &[flatzinc::Expr],
    constraint_tag: ConstraintTag,
) -> Result<(), FlatZincError> {
    // TODO: Take this constraint into account when creating variables, as these can be opposite
    // literals of the same PropositionalVariable. Unsure how often this actually appears in models
    // though.

    check_parameters!(exprs, 2, "bool_not");

    let a = context.resolve_bool_variable(&exprs[0])?;
    let b = context.resolve_bool_variable(&exprs[1])?;

    pumpkin_constraints::binary_not_equals(a, b, constraint_tag).post(context.solver);

    Ok(())
}

fn compile_bool_eq_reif(
    context: &mut CompilationContext<'_>,
    exprs: &[flatzinc::Expr],
    constraint_tag: ConstraintTag,
) -> Result<(), FlatZincError> {
    check_parameters!(exprs, 3, "bool_eq_reif");

    let a = context.resolve_bool_variable(&exprs[0])?;
    let b = context.resolve_bool_variable(&exprs[1])?;
    let r = context.resolve_bool_variable(&exprs[2])?;

    pumpkin_constraints::binary_equals(a, b, constraint_tag).reify(context.solver, r);

    Ok(())
}

fn compile_bool_eq(
    context: &mut CompilationContext<'_>,
    exprs: &[flatzinc::Expr],
    constraint_tag: ConstraintTag,
) -> Result<(), FlatZincError> {
    // TODO: Take this constraint into account when merging equivalence classes. Unsure how often
    // this actually appears in models though.
    check_parameters!(exprs, 2, "bool_eq");

    let a = context.resolve_bool_variable(&exprs[0])?;
    let b = context.resolve_bool_variable(&exprs[1])?;

    pumpkin_constraints::binary_equals(a, b, constraint_tag).post(context.solver);

    Ok(())
}

fn compile_bool_clause(
    context: &mut CompilationContext<'_>,
    exprs: &[flatzinc::Expr],
    constraint_tag: ConstraintTag,
) -> Result<(), FlatZincError> {
    check_parameters!(exprs, 2, "bool_clause");

    let clause_1 = context.resolve_bool_variable_array(&exprs[0])?;
    let clause_2 = context.resolve_bool_variable_array(&exprs[1])?;

    let clause: Vec<Predicate> = clause_1
        .iter()
        .cloned()
        .chain(clause_2.iter().map(|literal| !*literal))
        .map(|literal| literal.get_true_predicate())
        .collect();

    context.solver.add_clause(clause, constraint_tag);

    Ok(())
}

fn compile_bool_and(
    context: &mut CompilationContext<'_>,
    exprs: &[flatzinc::Expr],
    constraint_tag: ConstraintTag,
) -> Result<(), FlatZincError> {
    check_parameters!(exprs, 2, "bool_and");

    let a = context.resolve_bool_variable(&exprs[0])?;
    let b = context.resolve_bool_variable(&exprs[1])?;
    let r = context.resolve_bool_variable(&exprs[2])?;

    pumpkin_constraints::conjunction([a, b], constraint_tag).reify(context.solver, r);

    Ok(())
}

fn compile_bool2int(
    context: &mut CompilationContext<'_>,
    exprs: &[flatzinc::Expr],
    constraint_tag: ConstraintTag,
) -> Result<(), FlatZincError> {
    // TODO: Perhaps we want to add a phase in the compiler that directly uses the literal
    // corresponding to the predicate [b = 1] for the boolean parameter in this constraint.
    // See https://emir-demirovic.atlassian.net/browse/PUM-89

    check_parameters!(exprs, 2, "bool2int");

    let a = context.resolve_bool_variable(&exprs[0])?;
    let b = context.resolve_integer_variable(&exprs[1])?;

    pumpkin_constraints::binary_equals(a.get_integer_variable(), b.scaled(1), constraint_tag)
        .post(context.solver);

    Ok(())
}

fn compile_bool_or(
    context: &mut CompilationContext<'_>,
    exprs: &[flatzinc::Expr],
    constraint_tag: ConstraintTag,
) -> Result<(), FlatZincError> {
    check_parameters!(exprs, 2, "bool_or");

    let clause = context.resolve_bool_variable_array(&exprs[0])?;
    let r = context.resolve_bool_variable(&exprs[1])?;

    pumpkin_constraints::clause(clause.as_ref(), constraint_tag).reify(context.solver, r);

    Ok(())
}

fn compile_bool_xor(
    context: &mut CompilationContext<'_>,
    exprs: &[flatzinc::Expr],
    constraint_tag: ConstraintTag,
) -> Result<(), FlatZincError> {
    check_parameters!(exprs, 2, "pumpkin_bool_xor");

    let a = context
        .resolve_bool_variable(&exprs[0])?
        .get_true_predicate();
    let b = context
        .resolve_bool_variable(&exprs[1])?
        .get_true_predicate();

    context.solver.add_clause([!a, !b], constraint_tag);
    context.solver.add_clause([b, a], constraint_tag);

    Ok(())
}

fn compile_bool_xor_reif(
    context: &mut CompilationContext<'_>,
    exprs: &[flatzinc::Expr],
    constraint_tag: ConstraintTag,
) -> Result<(), FlatZincError> {
    check_parameters!(exprs, 3, "pumpkin_bool_xor_reif");

    let a = context.resolve_bool_variable(&exprs[0])?;
    let b = context.resolve_bool_variable(&exprs[1])?;
    let r = context.resolve_bool_variable(&exprs[2])?;

    pumpkin_constraints::clause([!a, !b, !r], constraint_tag).post(context.solver);
    pumpkin_constraints::clause([!a, b, r], constraint_tag).post(context.solver);
    pumpkin_constraints::clause([a, !b, r], constraint_tag).post(context.solver);
    pumpkin_constraints::clause([a, b, !r], constraint_tag).post(context.solver);

    Ok(())
}

fn compile_array_var_bool_element(
    context: &mut CompilationContext<'_>,
    exprs: &[flatzinc::Expr],
    name: &str,
    constraint_tag: ConstraintTag,
) -> Result<(), FlatZincError> {
    check_parameters!(exprs, 3, name);

    let index = context.resolve_integer_variable(&exprs[0])?.offset(-1);
    let array = context.resolve_bool_variable_array(&exprs[1])?;
    let rhs = context.resolve_bool_variable(&exprs[2])?;

    pumpkin_constraints::element(index, array.iter().cloned(), rhs, constraint_tag)
        .post(context.solver);

    Ok(())
}

fn compile_array_bool_and(
    context: &mut CompilationContext<'_>,
    exprs: &[flatzinc::Expr],
    constraint_tag: ConstraintTag,
) -> Result<(), FlatZincError> {
    check_parameters!(exprs, 2, "array_bool_and");

    let conjunction = context.resolve_bool_variable_array(&exprs[0])?;
    let r = context.resolve_bool_variable(&exprs[1])?;

    pumpkin_constraints::conjunction(conjunction.as_ref(), constraint_tag).reify(context.solver, r);

    Ok(())
}

fn compile_ternary_int_predicate<C: Constraint>(
    context: &mut CompilationContext,
    exprs: &[flatzinc::Expr],
    _: &[flatzinc::Annotation],
    predicate_name: &str,
    constraint_tag: ConstraintTag,
    create_constraint: impl FnOnce(DomainId, DomainId, DomainId, ConstraintTag) -> C,
) -> Result<(), FlatZincError> {
    check_parameters!(exprs, 3, predicate_name);

    let a = context.resolve_integer_variable(&exprs[0])?;
    let b = context.resolve_integer_variable(&exprs[1])?;
    let c = context.resolve_integer_variable(&exprs[2])?;

    let constraint = create_constraint(a, b, c, constraint_tag);
    constraint.post(context.solver);

    Ok(())
}

fn compile_binary_int_predicate<C: Constraint>(
    context: &mut CompilationContext,
    exprs: &[flatzinc::Expr],
    _: &[flatzinc::Annotation],
    predicate_name: &str,
    constraint_tag: ConstraintTag,
    create_constraint: impl FnOnce(DomainId, DomainId, ConstraintTag) -> C,
) -> Result<(), FlatZincError> {
    check_parameters!(exprs, 2, predicate_name);

    let a = context.resolve_integer_variable(&exprs[0])?;
    let b = context.resolve_integer_variable(&exprs[1])?;

    let constraint = create_constraint(a, b, constraint_tag);
    constraint.post(context.solver);

    Ok(())
}

fn compile_reified_binary_int_predicate<C: NegatableConstraint>(
    context: &mut CompilationContext,
    exprs: &[flatzinc::Expr],
    _: &[flatzinc::Annotation],
    predicate_name: &str,
    constraint_tag: ConstraintTag,
    create_constraint: impl FnOnce(DomainId, DomainId, ConstraintTag) -> C,
) -> Result<(), FlatZincError> {
    check_parameters!(exprs, 3, predicate_name);

    let a = context.resolve_integer_variable(&exprs[0])?;
    let b = context.resolve_integer_variable(&exprs[1])?;
    let reif = context.resolve_bool_variable(&exprs[2])?;

    let constraint = create_constraint(a, b, constraint_tag);
    constraint.reify(context.solver, reif);

    Ok(())
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
    predicate_name: &str,
    constraint_tag: ConstraintTag,
    create_constraint: impl FnOnce(Box<[AffineView<DomainId>]>, i32, ConstraintTag) -> C,
) -> Result<(), FlatZincError> {
    check_parameters!(exprs, 3, predicate_name);

    let weights = context.resolve_array_integer_constants(&exprs[0])?;
    let vars = context.resolve_integer_variable_array(&exprs[1])?;
    let rhs = context.resolve_integer_constant_from_expr(&exprs[2])?;

    let terms = weighted_vars(weights, vars);

    let constraint = create_constraint(terms, rhs, constraint_tag);
    constraint.post(context.solver);

    Ok(())
}

fn compile_reified_int_lin_predicate<C: NegatableConstraint>(
    context: &mut CompilationContext,
    exprs: &[flatzinc::Expr],
    _: &[flatzinc::Annotation],
    predicate_name: &str,
    constraint_tag: ConstraintTag,
    create_constraint: impl FnOnce(Box<[AffineView<DomainId>]>, i32, ConstraintTag) -> C,
) -> Result<(), FlatZincError> {
    check_parameters!(exprs, 4, predicate_name);

    let weights = context.resolve_array_integer_constants(&exprs[0])?;
    let vars = context.resolve_integer_variable_array(&exprs[1])?;
    let rhs = context.resolve_integer_constant_from_expr(&exprs[2])?;
    let reif = context.resolve_bool_variable(&exprs[3])?;

    let terms = weighted_vars(weights, vars);

    let constraint = create_constraint(terms, rhs, constraint_tag);
    constraint.reify(context.solver, reif);

    Ok(())
}

fn compile_int_lin_imp_predicate<C: Constraint>(
    context: &mut CompilationContext,
    exprs: &[flatzinc::Expr],
    _: &[flatzinc::Annotation],
    predicate_name: &str,
    constraint_tag: ConstraintTag,
    create_constraint: impl FnOnce(Box<[AffineView<DomainId>]>, i32, ConstraintTag) -> C,
) -> Result<(), FlatZincError> {
    check_parameters!(exprs, 4, predicate_name);

    let weights = context.resolve_array_integer_constants(&exprs[0])?;
    let vars = context.resolve_integer_variable_array(&exprs[1])?;
    let rhs = context.resolve_integer_constant_from_expr(&exprs[2])?;
    let reif = context.resolve_bool_variable(&exprs[3])?;

    let terms = weighted_vars(weights, vars);

    let constraint = create_constraint(terms, rhs, constraint_tag);
    constraint.implied_by(context.solver, reif);

    Ok(())
}

fn compile_binary_int_imp<C: Constraint>(
    context: &mut CompilationContext,
    exprs: &[flatzinc::Expr],
    _: &[flatzinc::Annotation],
    predicate_name: &str,
    constraint_tag: ConstraintTag,
    create_constraint: impl FnOnce(DomainId, DomainId, ConstraintTag) -> C,
) -> Result<(), FlatZincError> {
    check_parameters!(exprs, 3, predicate_name);

    let a = context.resolve_integer_variable(&exprs[0])?;
    let b = context.resolve_integer_variable(&exprs[1])?;
    let reif = context.resolve_bool_variable(&exprs[2])?;

    let constraint = create_constraint(a, b, constraint_tag);
    constraint.implied_by(context.solver, reif);

    Ok(())
}

fn compile_bool_lin_eq_predicate(
    context: &mut CompilationContext,
    exprs: &[flatzinc::Expr],
    constraint_tag: ConstraintTag,
) -> Result<(), FlatZincError> {
    check_parameters!(exprs, 3, "bool_lin_eq");

    let weights = context.resolve_array_integer_constants(&exprs[0])?;
    let bools = context.resolve_bool_variable_array(&exprs[1])?;
    let rhs = context.resolve_integer_variable(&exprs[2])?;

    pumpkin_constraints::boolean_equals(
        weights.as_ref().to_owned(),
        bools.as_ref().to_owned(),
        rhs,
        constraint_tag,
    )
    .post(context.solver);

    Ok(())
}

fn compile_bool_lin_le_predicate(
    context: &mut CompilationContext,
    exprs: &[flatzinc::Expr],
    constraint_tag: ConstraintTag,
) -> Result<(), FlatZincError> {
    check_parameters!(exprs, 3, "bool_lin_le");

    let weights = context.resolve_array_integer_constants(&exprs[0])?;
    let bools = context.resolve_bool_variable_array(&exprs[1])?;
    let rhs = context.resolve_integer_constant_from_expr(&exprs[2])?;

    pumpkin_constraints::boolean_less_than_or_equals(
        weights.as_ref().to_owned(),
        bools.as_ref().to_owned(),
        rhs,
        constraint_tag,
    )
    .post(context.solver);

    Ok(())
}

fn compile_all_different(
    context: &mut CompilationContext,
    exprs: &[flatzinc::Expr],
    _: &[flatzinc::Annotation],
    constraint_tag: ConstraintTag,
) -> Result<(), FlatZincError> {
    check_parameters!(exprs, 1, "fzn_all_different");

    let variables = context.resolve_integer_variable_array(&exprs[0])?.to_vec();
    // The disequalities it is decomposed into are cited as the Hall rule on the pair.
    context.solver.name_positions_of_constraint(
        constraint_tag,
        variables
            .iter()
            .map(|variable| variable.scaled(1))
            .collect(),
    );
    pumpkin_constraints::all_different(variables, constraint_tag).post(context.solver);

    Ok(())
}

fn compile_table(
    context: &mut CompilationContext,
    exprs: &[flatzinc::Expr],
    _: &[flatzinc::Annotation],
    constraint_tag: ConstraintTag,
) -> Result<(), FlatZincError> {
    check_parameters!(exprs, 2, "pumpkin_table_int");

    let variables = context.resolve_integer_variable_array(&exprs[0])?.to_vec();

    let flat_table = context.resolve_array_integer_constants(&exprs[1])?;
    let table = create_table(flat_table, variables.len());
    record_table_decomposition(context, constraint_tag, table.len());

    pumpkin_constraints::table(variables, table, constraint_tag).post(context.solver);

    Ok(())
}

fn compile_table_reif(
    context: &mut CompilationContext,
    exprs: &[flatzinc::Expr],
    _: &[flatzinc::Annotation],
    constraint_tag: ConstraintTag,
) -> Result<(), FlatZincError> {
    check_parameters!(exprs, 3, "pumpkin_table_int_reif");

    let variables = context.resolve_integer_variable_array(&exprs[0])?.to_vec();

    let flat_table = context.resolve_array_integer_constants(&exprs[1])?;
    let table = create_table(flat_table, variables.len());

    let reified = context.resolve_bool_variable(&exprs[2])?;
    record_table_decomposition(context, constraint_tag, table.len());

    pumpkin_constraints::table(variables, table, constraint_tag).reify(context.solver, reified);

    Ok(())
}

/// Under proof logging a table is posted as its decomposition, with a named selector per row,
/// which the preamble states.
fn record_table_decomposition(
    context: &mut CompilationContext,
    constraint_tag: ConstraintTag,
    rows: usize,
) {
    if !context.solver.is_logging_proof() {
        return;
    }
    context.preamble.decompositions.push(Decomposition {
        kind: DecompositionKind::Table,
        constraint: u64::from(std::num::NonZero::<u32>::from(constraint_tag).get()),
        fresh_variables: (0..rows)
            .map(|row| pumpkin_constraints::table_selector_name(constraint_tag, row))
            .collect(),
    });
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
