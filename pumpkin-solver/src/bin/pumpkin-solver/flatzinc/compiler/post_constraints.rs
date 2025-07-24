//! Compile constraints into CP propagators

use fzn_rs::VariableExpr;
use pumpkin_core::variables::Literal;
use pumpkin_solver::constraints;
use pumpkin_solver::constraints::Constraint;
use pumpkin_solver::constraints::NegatableConstraint;
use pumpkin_solver::predicate;
use pumpkin_solver::predicates::Predicate;
use pumpkin_solver::proof::ConstraintTag;
use pumpkin_solver::variables::AffineView;
use pumpkin_solver::variables::DomainId;
use pumpkin_solver::variables::TransformableVariable;

use super::context::CompilationContext;
use crate::flatzinc::ast::ConstraintAnnotations;
use crate::flatzinc::ast::Instance;
use crate::flatzinc::constraints::ArrayBoolArgs;
use crate::flatzinc::constraints::Binary;
use crate::flatzinc::constraints::BinaryBool;
use crate::flatzinc::constraints::BinaryBoolReif;
use crate::flatzinc::constraints::BoolClauseArgs;
use crate::flatzinc::constraints::BoolElementArgs;
use crate::flatzinc::constraints::BoolLinEqArgs;
use crate::flatzinc::constraints::BoolLinLeArgs;
use crate::flatzinc::constraints::BoolToIntArgs;
use crate::flatzinc::constraints::Constraints;
use crate::flatzinc::constraints::CumulativeArgs;
use crate::flatzinc::constraints::IntElementArgs;
use crate::flatzinc::constraints::Linear;
use crate::flatzinc::constraints::ReifiedBinary;
use crate::flatzinc::constraints::ReifiedLinear;
use crate::flatzinc::constraints::SetInReifArgs;
use crate::flatzinc::constraints::TableInt;
use crate::flatzinc::constraints::TableIntReif;
use crate::flatzinc::constraints::TernaryIntArgs;
use crate::flatzinc::FlatZincError;
use crate::flatzinc::FlatZincOptions;

pub(crate) fn run(
    instance: &Instance,
    context: &mut CompilationContext,
    options: &FlatZincOptions,
) -> Result<(), FlatZincError> {
    use Constraints::*;

    for constraint in &instance.constraints {
        let constraint_tag = constraint
            .annotations
            .iter()
            .find_map(|ann| match &ann.node {
                ConstraintAnnotations::ConstraintTag(tag) => Some((*tag).into()),
            })
            .expect("every constraint should have been associated with a tag at an earlier stage");

        let is_satisfiable: bool = match &constraint.constraint.node {
            ArrayIntMinimum(args) => {
                let array = context.resolve_integer_variable_array(&args.array)?;
                let rhs = context.resolve_integer_variable(&args.extremum)?;

                constraints::minimum(array, rhs, constraint_tag)
                    .post(context.solver)
                    .is_ok()
            }

            ArrayIntMaximum(args) => {
                let array = context.resolve_integer_variable_array(&args.array)?;
                let rhs = context.resolve_integer_variable(&args.extremum)?;

                constraints::maximum(array, rhs, constraint_tag)
                    .post(context.solver)
                    .is_ok()
            }

            // We rewrite `array_int_element` to `array_var_int_element`.
            ArrayIntElement(args) => compile_array_var_int_element(context, args, constraint_tag)?,
            ArrayVarIntElement(args) => {
                compile_array_var_int_element(context, args, constraint_tag)?
            }

            IntEqImp(args) => {
                compile_binary_int_imp(context, args, constraint_tag, constraints::binary_equals)?
            }
            IntGeImp(args) => compile_binary_int_imp(
                context,
                args,
                constraint_tag,
                constraints::binary_greater_than_or_equals,
            )?,
            IntGtImp(args) => compile_binary_int_imp(
                context,
                args,
                constraint_tag,
                constraints::binary_greater_than,
            )?,
            IntLeImp(args) => compile_binary_int_imp(
                context,
                args,
                constraint_tag,
                constraints::binary_less_than_or_equals,
            )?,
            IntLtImp(args) => compile_binary_int_imp(
                context,
                args,
                constraint_tag,
                constraints::binary_less_than,
            )?,
            IntNeImp(args) => compile_binary_int_imp(
                context,
                args,
                constraint_tag,
                constraints::binary_not_equals,
            )?,

            IntLinNe(args) => {
                compile_int_lin_predicate(context, args, constraint_tag, constraints::not_equals)?
            }
            IntLinLe(args) => compile_int_lin_predicate(
                context,
                args,
                constraint_tag,
                constraints::less_than_or_equals,
            )?,
            IntLinEq(args) => {
                compile_int_lin_predicate(context, args, constraint_tag, constraints::equals)?
            }

            IntLinNeReif(args) => compile_reified_int_lin_predicate(
                context,
                args,
                constraint_tag,
                constraints::not_equals,
            )?,
            IntLinLeReif(args) => compile_reified_int_lin_predicate(
                context,
                args,
                constraint_tag,
                constraints::less_than_or_equals,
            )?,
            IntLinEqReif(args) => compile_reified_int_lin_predicate(
                context,
                args,
                constraint_tag,
                constraints::equals,
            )?,

            IntEq(args) => compile_binary_int_predicate(
                context,
                args,
                constraint_tag,
                constraints::binary_equals,
            )?,
            IntNe(args) => compile_binary_int_predicate(
                context,
                args,
                constraint_tag,
                constraints::binary_equals,
            )?,
            IntLe(args) => compile_binary_int_predicate(
                context,
                args,
                constraint_tag,
                constraints::binary_less_than_or_equals,
            )?,
            IntLt(args) => compile_binary_int_predicate(
                context,
                args,
                constraint_tag,
                constraints::binary_less_than,
            )?,
            IntAbs(args) => {
                compile_binary_int_predicate(context, args, constraint_tag, constraints::absolute)?
            }

            IntEqReif(args) => compile_reified_binary_int_predicate(
                context,
                args,
                constraint_tag,
                constraints::binary_equals,
            )?,
            IntNeReif(args) => compile_reified_binary_int_predicate(
                context,
                args,
                constraint_tag,
                constraints::binary_equals,
            )?,
            IntLtReif(args) => compile_reified_binary_int_predicate(
                context,
                args,
                constraint_tag,
                constraints::binary_equals,
            )?,
            IntLeReif(args) => compile_reified_binary_int_predicate(
                context,
                args,
                constraint_tag,
                constraints::binary_equals,
            )?,

            IntMax(args) => compile_ternary_int_predicate(
                context,
                args,
                constraint_tag,
                |a, b, c, constraint_tag| constraints::maximum([a, b], c, constraint_tag),
            )?,

            IntMin(args) => compile_ternary_int_predicate(
                context,
                args,
                constraint_tag,
                |a, b, c, constraint_tag| constraints::maximum([a, b], c, constraint_tag),
            )?,

            IntTimes(args) => {
                compile_ternary_int_predicate(context, args, constraint_tag, constraints::times)?
            }
            IntDiv(args) => {
                compile_ternary_int_predicate(context, args, constraint_tag, constraints::division)?
            }
            IntPlus(args) => {
                compile_ternary_int_predicate(context, args, constraint_tag, constraints::plus)?
            }

            AllDifferent(array) => {
                let variables = context.resolve_integer_variable_array(array)?;
                constraints::all_different(variables, constraint_tag)
                    .post(context.solver)
                    .is_ok()
            }

            Table(table) => compile_table(context, table, constraint_tag)?,
            TableReif(table_reif) => compile_table_reif(context, table_reif, constraint_tag)?,

            ArrayBoolAnd(args) => {
                compile_array_bool(context, args, constraint_tag, constraints::conjunction)?
            }

            ArrayBoolOr(args) => {
                compile_array_bool(context, args, constraint_tag, constraints::clause)?
            }

            BoolXor(args) => compile_bool_xor(context, args, constraint_tag)?,
            BoolXorReif(args) => compile_bool_xor_reif(context, args, constraint_tag)?,

            BoolLinEq(args) => compile_bool_lin_eq_predicate(context, args, constraint_tag)?,
            BoolLinLe(args) => compile_bool_lin_le_predicate(context, args, constraint_tag)?,

            BoolAnd(args) => compile_bool_and(context, args, constraint_tag)?,
            BoolEq(args) => compile_bool_eq(context, args, constraint_tag)?,
            BoolEqReif(args) => compile_bool_eq_reif(context, args, constraint_tag)?,
            BoolNot(args) => compile_bool_not(context, args, constraint_tag)?,
            BoolClause(args) => compile_bool_clause(context, args, constraint_tag)?,

            ArrayBoolElement(args) => {
                compile_array_var_bool_element(context, args, constraint_tag)?
            }
            ArrayVarBoolElement(args) => {
                compile_array_var_bool_element(context, args, constraint_tag)?
            }

            BoolToInt(args) => compile_bool2int(context, args, constraint_tag)?,

            SetIn(_, _) => {
                unreachable!("should be removed from the AST at previous stages")
            }

            SetInReif(args) => compile_set_in_reif(context, args, constraint_tag)?,

            Cumulative(args) => compile_cumulative(context, args, options, constraint_tag)?,
        };

        if !is_satisfiable {
            break;
        }
    }

    Ok(())
}

fn compile_cumulative(
    context: &mut CompilationContext<'_>,
    args: &CumulativeArgs,
    options: &FlatZincOptions,
    constraint_tag: ConstraintTag,
) -> Result<bool, FlatZincError> {
    let start_times = context.resolve_integer_variable_array(&args.start_times)?;

    let post_result = constraints::cumulative_with_options(
        start_times,
        args.durations.clone(),
        args.resource_requirements.clone(),
        args.resource_capacity,
        options.cumulative_options,
        constraint_tag,
    )
    .post(context.solver);
    Ok(post_result.is_ok())
}

fn compile_set_in_reif(
    context: &mut CompilationContext<'_>,
    args: &SetInReifArgs,
    constraint_tag: ConstraintTag,
) -> Result<bool, FlatZincError> {
    let variable = context.resolve_integer_variable(&args.variable)?;
    let reif = context.resolve_bool_variable(&args.reification)?;

    let success = if args.set.is_continuous() {
        // `reif -> x \in S`
        // Decomposed to `reif -> x >= lb /\ reif -> x <= ub`
        let forward = context
            .solver
            .add_clause(
                [
                    !reif.get_true_predicate(),
                    predicate![variable >= *args.set.lower_bound()],
                ],
                constraint_tag,
            )
            .is_ok()
            && context
                .solver
                .add_clause(
                    [
                        !reif.get_true_predicate(),
                        !predicate![variable >= *args.set.upper_bound() + 1],
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
                    !predicate![variable >= *args.set.lower_bound()],
                    predicate![variable >= *args.set.upper_bound() + 1],
                ],
                constraint_tag,
            )
            .is_ok();

        forward && backward
    } else {
        let clause = args
            .set
            .into_iter()
            .map(|value| {
                context
                    .solver
                    .new_literal_for_predicate(predicate![variable == value], constraint_tag)
            })
            .collect::<Vec<_>>();

        constraints::clause(clause, constraint_tag)
            .reify(context.solver, reif)
            .is_ok()
    };

    Ok(success)
}

fn compile_array_var_int_element(
    context: &mut CompilationContext<'_>,
    args: &IntElementArgs,
    constraint_tag: ConstraintTag,
) -> Result<bool, FlatZincError> {
    let index = context.resolve_integer_variable(&args.index)?.offset(-1);
    let array = context.resolve_integer_variable_array(&args.array)?;
    let rhs = context.resolve_integer_variable(&args.rhs)?;

    Ok(constraints::element(index, array, rhs, constraint_tag)
        .post(context.solver)
        .is_ok())
}

fn compile_bool_not(
    context: &mut CompilationContext<'_>,
    args: &BinaryBool,
    constraint_tag: ConstraintTag,
) -> Result<bool, FlatZincError> {
    let a = context.resolve_bool_variable(&args.a)?;
    let b = context.resolve_bool_variable(&args.b)?;

    Ok(constraints::binary_not_equals(a, b, constraint_tag)
        .post(context.solver)
        .is_ok())
}

fn compile_bool_eq_reif(
    context: &mut CompilationContext<'_>,
    args: &BinaryBoolReif,
    constraint_tag: ConstraintTag,
) -> Result<bool, FlatZincError> {
    let a = context.resolve_bool_variable(&args.a)?;
    let b = context.resolve_bool_variable(&args.b)?;
    let r = context.resolve_bool_variable(&args.reification)?;

    Ok(constraints::binary_equals(a, b, constraint_tag)
        .reify(context.solver, r)
        .is_ok())
}

fn compile_bool_eq(
    context: &mut CompilationContext<'_>,
    args: &BinaryBool,
    constraint_tag: ConstraintTag,
) -> Result<bool, FlatZincError> {
    let a = context.resolve_bool_variable(&args.a)?;
    let b = context.resolve_bool_variable(&args.b)?;

    Ok(constraints::binary_equals(a, b, constraint_tag)
        .post(context.solver)
        .is_ok())
}

fn compile_bool_clause(
    context: &mut CompilationContext<'_>,
    args: &BoolClauseArgs,
    constraint_tag: ConstraintTag,
) -> Result<bool, FlatZincError> {
    let clause_1 = context.resolve_bool_variable_array(&args.clause_1)?;
    let clause_2 = context.resolve_bool_variable_array(&args.clause_2)?;

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
    args: &BinaryBoolReif,
    constraint_tag: ConstraintTag,
) -> Result<bool, FlatZincError> {
    let a = context.resolve_bool_variable(&args.a)?;
    let b = context.resolve_bool_variable(&args.b)?;
    let r = context.resolve_bool_variable(&args.reification)?;

    Ok(constraints::conjunction([a, b], constraint_tag)
        .reify(context.solver, r)
        .is_ok())
}

fn compile_bool2int(
    context: &mut CompilationContext<'_>,
    args: &BoolToIntArgs,
    constraint_tag: ConstraintTag,
) -> Result<bool, FlatZincError> {
    let a = context.resolve_bool_variable(&args.boolean)?;
    let b = context.resolve_integer_variable(&args.integer)?;

    Ok(
        constraints::binary_equals(a.get_integer_variable(), b.scaled(1), constraint_tag)
            .post(context.solver)
            .is_ok(),
    )
}

fn compile_bool_or(
    context: &mut CompilationContext<'_>,
    args: &ArrayBoolArgs,
    constraint_tag: ConstraintTag,
) -> Result<bool, FlatZincError> {
    let clause = context.resolve_bool_variable_array(&args.booleans)?;
    let r = context.resolve_bool_variable(&args.reification)?;

    Ok(constraints::clause(clause, constraint_tag)
        .reify(context.solver, r)
        .is_ok())
}

fn compile_bool_xor(
    context: &mut CompilationContext<'_>,
    args: &BinaryBool,
    constraint_tag: ConstraintTag,
) -> Result<bool, FlatZincError> {
    let a = context.resolve_bool_variable(&args.a)?.get_true_predicate();
    let b = context.resolve_bool_variable(&args.b)?.get_true_predicate();

    let c1 = context.solver.add_clause([!a, !b], constraint_tag).is_ok();
    let c2 = context.solver.add_clause([b, a], constraint_tag).is_ok();

    Ok(c1 && c2)
}

fn compile_bool_xor_reif(
    context: &mut CompilationContext<'_>,
    args: &BinaryBoolReif,
    constraint_tag: ConstraintTag,
) -> Result<bool, FlatZincError> {
    let a = context.resolve_bool_variable(&args.a)?;
    let b = context.resolve_bool_variable(&args.b)?;
    let r = context.resolve_bool_variable(&args.reification)?;

    let c1 = constraints::clause([!a, !b, !r], constraint_tag)
        .post(context.solver)
        .is_ok();
    let c2 = constraints::clause([!a, b, r], constraint_tag)
        .post(context.solver)
        .is_ok();
    let c3 = constraints::clause([a, !b, r], constraint_tag)
        .post(context.solver)
        .is_ok();
    let c4 = constraints::clause([a, b, !r], constraint_tag)
        .post(context.solver)
        .is_ok();

    Ok(c1 && c2 && c3 && c4)
}

fn compile_array_var_bool_element(
    context: &mut CompilationContext<'_>,
    args: &BoolElementArgs,
    constraint_tag: ConstraintTag,
) -> Result<bool, FlatZincError> {
    let index = context.resolve_integer_variable(&args.index)?.offset(-1);
    let array = context.resolve_bool_variable_array(&args.array)?;
    let rhs = context.resolve_bool_variable(&args.rhs)?;

    Ok(
        constraints::element(index, array.iter().cloned(), rhs, constraint_tag)
            .post(context.solver)
            .is_ok(),
    )
}

fn compile_array_bool<C: NegatableConstraint>(
    context: &mut CompilationContext<'_>,
    args: &ArrayBoolArgs,
    constraint_tag: ConstraintTag,
    create_constraint: impl FnOnce(Vec<Literal>, ConstraintTag) -> C,
) -> Result<bool, FlatZincError> {
    let conjunction = context.resolve_bool_variable_array(&args.booleans)?;
    let r = context.resolve_bool_variable(&args.reification)?;

    Ok(create_constraint(conjunction, constraint_tag)
        .reify(context.solver, r)
        .is_ok())
}

fn compile_ternary_int_predicate<C: Constraint>(
    context: &mut CompilationContext,
    ternary_int_args: &TernaryIntArgs,
    constraint_tag: ConstraintTag,
    create_constraint: impl FnOnce(DomainId, DomainId, DomainId, ConstraintTag) -> C,
) -> Result<bool, FlatZincError> {
    let a = context.resolve_integer_variable(&ternary_int_args.a)?;
    let b = context.resolve_integer_variable(&ternary_int_args.b)?;
    let c = context.resolve_integer_variable(&ternary_int_args.c)?;

    let constraint = create_constraint(a, b, c, constraint_tag);
    Ok(constraint.post(context.solver).is_ok())
}

fn compile_binary_int_predicate<C: Constraint>(
    context: &mut CompilationContext,
    Binary(lhs, rhs): &Binary,
    constraint_tag: ConstraintTag,
    create_constraint: impl FnOnce(DomainId, DomainId, ConstraintTag) -> C,
) -> Result<bool, FlatZincError> {
    let a = context.resolve_integer_variable(lhs)?;
    let b = context.resolve_integer_variable(rhs)?;

    let constraint = create_constraint(a, b, constraint_tag);
    Ok(constraint.post(context.solver).is_ok())
}

fn compile_reified_binary_int_predicate<C: NegatableConstraint>(
    context: &mut CompilationContext,
    args: &ReifiedBinary,
    constraint_tag: ConstraintTag,
    create_constraint: impl FnOnce(DomainId, DomainId, ConstraintTag) -> C,
) -> Result<bool, FlatZincError> {
    let a = context.resolve_integer_variable(&args.a)?;
    let b = context.resolve_integer_variable(&args.a)?;
    let reif = context.resolve_bool_variable(&args.reification)?;

    let constraint = create_constraint(a, b, constraint_tag);
    Ok(constraint.reify(context.solver, reif).is_ok())
}

fn weighted_vars(weights: &[i32], vars: Vec<DomainId>) -> Box<[AffineView<DomainId>]> {
    vars.iter()
        .zip(weights.iter())
        .map(|(x_i, &w_i)| x_i.scaled(w_i))
        .collect::<Box<[_]>>()
}

fn compile_int_lin_predicate<C: Constraint>(
    context: &mut CompilationContext,
    args: &Linear,
    constraint_tag: ConstraintTag,
    create_constraint: impl FnOnce(Box<[AffineView<DomainId>]>, i32, ConstraintTag) -> C,
) -> Result<bool, FlatZincError> {
    let vars = context.resolve_integer_variable_array(&args.variables)?;
    let terms = weighted_vars(&args.weights, vars);

    let constraint = create_constraint(terms, args.rhs, constraint_tag);
    Ok(constraint.post(context.solver).is_ok())
}

fn compile_reified_int_lin_predicate<C: NegatableConstraint>(
    context: &mut CompilationContext,
    args: &ReifiedLinear,
    constraint_tag: ConstraintTag,
    create_constraint: impl FnOnce(Box<[AffineView<DomainId>]>, i32, ConstraintTag) -> C,
) -> Result<bool, FlatZincError> {
    let vars = context.resolve_integer_variable_array(&args.variables)?;
    let reif = context.resolve_bool_variable(&args.reification)?;

    let terms = weighted_vars(&args.weights, vars);

    let constraint = create_constraint(terms, args.rhs, constraint_tag);
    Ok(constraint.reify(context.solver, reif).is_ok())
}

fn compile_binary_int_imp<C: Constraint>(
    context: &mut CompilationContext,
    args: &ReifiedBinary,
    constraint_tag: ConstraintTag,
    create_constraint: impl FnOnce(DomainId, DomainId, ConstraintTag) -> C,
) -> Result<bool, FlatZincError> {
    let a = context.resolve_integer_variable(&args.a)?;
    let b = context.resolve_integer_variable(&args.b)?;
    let reif = context.resolve_bool_variable(&args.reification)?;

    let constraint = create_constraint(a, b, constraint_tag);
    Ok(constraint.implied_by(context.solver, reif).is_ok())
}

fn compile_bool_lin_eq_predicate(
    context: &mut CompilationContext,
    args: &BoolLinEqArgs,
    constraint_tag: ConstraintTag,
) -> Result<bool, FlatZincError> {
    let bools = context.resolve_bool_variable_array(&args.variables)?;
    let rhs = context.resolve_integer_variable(&args.sum)?;

    Ok(
        constraints::boolean_equals(args.weights.clone(), bools, rhs, constraint_tag)
            .post(context.solver)
            .is_ok(),
    )
}

fn compile_bool_lin_le_predicate(
    context: &mut CompilationContext,
    args: &BoolLinLeArgs,
    constraint_tag: ConstraintTag,
) -> Result<bool, FlatZincError> {
    let bools = context.resolve_bool_variable_array(&args.variables)?;

    Ok(constraints::boolean_less_than_or_equals(
        args.weights.clone(),
        bools,
        args.bound,
        constraint_tag,
    )
    .post(context.solver)
    .is_ok())
}

fn compile_all_different(
    context: &mut CompilationContext,
    array: &[VariableExpr<i32>],
    constraint_tag: ConstraintTag,
) -> Result<bool, FlatZincError> {
    let variables = context.resolve_integer_variable_array(array)?;
    Ok(constraints::all_different(variables, constraint_tag)
        .post(context.solver)
        .is_ok())
}

fn compile_table(
    context: &mut CompilationContext,
    table: &TableInt,
    constraint_tag: ConstraintTag,
) -> Result<bool, FlatZincError> {
    let variables = context.resolve_integer_variable_array(&table.variables)?;
    let table = create_table(&table.table, variables.len());

    Ok(constraints::table(variables, table, constraint_tag)
        .post(context.solver)
        .is_ok())
}

fn compile_table_reif(
    context: &mut CompilationContext,
    table_reif: &TableIntReif,
    constraint_tag: ConstraintTag,
) -> Result<bool, FlatZincError> {
    let variables = context
        .resolve_integer_variable_array(&table_reif.variables)?
        .to_vec();
    let table = create_table(&table_reif.table, variables.len());
    let reified = context.resolve_bool_variable(&table_reif.reification)?;

    Ok(constraints::table(variables, table, constraint_tag)
        .reify(context.solver, reified)
        .is_ok())
}

fn create_table(flat_table: &[i32], num_variables: usize) -> Vec<Vec<i32>> {
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
