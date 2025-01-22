//! Compile constraints into CP propagators

use std::rc::Rc;

use pumpkin_solver::constraints;
use pumpkin_solver::constraints::Constraint;
use pumpkin_solver::constraints::NegatableConstraint;
use pumpkin_solver::predicate;
use pumpkin_solver::predicates::Predicate;
use pumpkin_solver::variables::AffineView;
use pumpkin_solver::variables::DomainId;
use pumpkin_solver::variables::TransformableVariable;

use super::context::CompilationContext;
use crate::flatzinc::ast::FlatZincAst;
use crate::flatzinc::compiler::context::Set;
use crate::flatzinc::FlatZincError;
use crate::flatzinc::FlatZincOptions;

pub(crate) fn run(
    ast: &FlatZincAst,
    context: &mut CompilationContext,
    options: FlatZincOptions,
) -> Result<(), FlatZincError> {
    for constraint_item in &ast.constraint_decls {
        let flatzinc::ConstraintItem { id, exprs, annos } = constraint_item;

        let is_satisfiable: bool = match id.as_str() {
            "array_int_maximum" => compile_array_int_maximum(context, exprs)?,
            "array_int_minimum" => compile_array_int_minimum(context, exprs)?,
            "int_max" => {
                compile_ternary_int_predicate(context, exprs, annos, "int_max", |a, b, c| {
                    constraints::maximum([a, b], c)
                })?
            }
            "int_min" => {
                compile_ternary_int_predicate(context, exprs, annos, "int_min", |a, b, c| {
                    constraints::minimum([a, b], c)
                })?
            }

            // We rewrite `array_int_element` to `array_var_int_element`.
            "array_int_element" => compile_array_var_int_element(context, exprs)?,
            "array_var_int_element" => compile_array_var_int_element(context, exprs)?,

            "int_lin_ne" => compile_int_lin_predicate(
                context,
                exprs,
                annos,
                "int_lin_ne",
                constraints::not_equals,
            )?,
            "int_lin_ne_reif" => compile_reified_int_lin_predicate(
                context,
                exprs,
                annos,
                "int_lin_ne_reif",
                constraints::not_equals,
            )?,
            "int_lin_le" => compile_int_lin_predicate(
                context,
                exprs,
                annos,
                "int_lin_le",
                constraints::less_than_or_equals,
            )?,
            "int_lin_le_reif" => compile_reified_int_lin_predicate(
                context,
                exprs,
                annos,
                "int_lin_le_reif",
                constraints::less_than_or_equals,
            )?,
            "int_lin_eq" => {
                compile_int_lin_predicate(context, exprs, annos, "int_lin_eq", constraints::equals)?
            }
            "int_lin_eq_reif" => compile_reified_int_lin_predicate(
                context,
                exprs,
                annos,
                "int_lin_eq_reif",
                constraints::equals,
            )?,
            "int_ne" => compile_binary_int_predicate(
                context,
                exprs,
                annos,
                "int_ne",
                constraints::binary_not_equals,
            )?,
            "int_ne_reif" => compile_reified_binary_int_predicate(
                context,
                exprs,
                annos,
                "int_ne_reif",
                constraints::binary_not_equals,
            )?,
            "int_eq" => compile_binary_int_predicate(
                context,
                exprs,
                annos,
                "int_eq",
                constraints::binary_equals,
            )?,
            "int_eq_reif" => compile_reified_binary_int_predicate(
                context,
                exprs,
                annos,
                "int_eq_reif",
                constraints::binary_equals,
            )?,
            "int_le" => compile_binary_int_predicate(
                context,
                exprs,
                annos,
                "int_le",
                constraints::binary_less_than_or_equals,
            )?,
            "int_le_reif" => compile_reified_binary_int_predicate(
                context,
                exprs,
                annos,
                "int_le_reif",
                constraints::binary_less_than_or_equals,
            )?,
            "int_lt" => compile_binary_int_predicate(
                context,
                exprs,
                annos,
                "int_lt",
                constraints::binary_less_than,
            )?,
            "int_lt_reif" => compile_reified_binary_int_predicate(
                context,
                exprs,
                annos,
                "int_lt_reif",
                constraints::binary_less_than,
            )?,

            "int_plus" => {
                compile_ternary_int_predicate(context, exprs, annos, "int_plus", constraints::plus)?
            }

            "int_times" => compile_ternary_int_predicate(
                context,
                exprs,
                annos,
                "int_times",
                constraints::times,
            )?,
            "int_div" => compile_ternary_int_predicate(
                context,
                exprs,
                annos,
                "int_div",
                constraints::division,
            )?,
            "int_abs" => compile_binary_int_predicate(
                context,
                exprs,
                annos,
                "int_abs",
                constraints::absolute,
            )?,

            "pumpkin_all_different" => compile_all_different(context, exprs, annos)?,

            "array_bool_and" => compile_array_bool_and(context, exprs)?,
            "array_bool_element" => {
                compile_array_var_bool_element(context, exprs, "array_bool_element")?
            }
            "array_var_bool_element" => {
                compile_array_var_bool_element(context, exprs, "array_var_bool_element")?
            }
            "array_bool_or" => compile_bool_or(context, exprs)?,
            "pumpkin_bool_xor" => compile_bool_xor(context, exprs)?,
            "pumpkin_bool_xor_reif" => compile_bool_xor_reif(context, exprs)?,

            "bool2int" => compile_bool2int(context, exprs)?,

            "bool_lin_eq" => {
                compile_bool_lin_eq_predicate(context, exprs)?
            }

            "bool_lin_le" => {
                compile_bool_lin_le_predicate(context, exprs)?
            }

            "bool_and" => compile_bool_and(context, exprs)?,
            "bool_clause" => compile_bool_clause(context, exprs)?,
            "bool_eq" => compile_bool_eq(context, exprs)?,
            "bool_eq_reif" => compile_bool_eq_reif(context, exprs)?,
            "bool_not" => compile_bool_not(context, exprs)?,
            "set_in_reif" => compile_set_in_reif(context, exprs)?,
            "set_in" => {
                // 'set_in' constraints are handled in pre-processing steps.
                // TODO: remove it from the AST, so it does not need to be matched here
                true
            }

            "pumpkin_cumulative" => compile_cumulative(context, exprs, &options)?,
            "pumpkin_cumulative_var" => todo!("The `cumulative` constraint with variable duration/resource consumption/bound is not implemented yet!"),
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

fn compile_cumulative(
    context: &mut CompilationContext<'_>,
    exprs: &[flatzinc::Expr],
    options: &FlatZincOptions,
) -> Result<bool, FlatZincError> {
    check_parameters!(exprs, 4, "pumpkin_cumulative");

    let start_times = context.resolve_integer_variable_array(&exprs[0])?;
    let durations = context.resolve_array_integer_constants(&exprs[1])?;
    let resource_requirements = context.resolve_array_integer_constants(&exprs[2])?;
    let resource_capacity = context.resolve_integer_constant_from_expr(&exprs[3])?;

    let post_result = constraints::cumulative_with_options(
        start_times.iter().copied(),
        durations.iter().copied(),
        resource_requirements.iter().copied(),
        resource_capacity,
        options.cumulative_options.clone(),
    )
    .post(context.solver, None);
    Ok(post_result.is_ok())
}

fn compile_array_int_maximum(
    context: &mut CompilationContext<'_>,
    exprs: &[flatzinc::Expr],
) -> Result<bool, FlatZincError> {
    check_parameters!(exprs, 2, "array_int_maximum");

    let rhs = context.resolve_integer_variable(&exprs[0])?;
    let array = context.resolve_integer_variable_array(&exprs[1])?;

    Ok(constraints::maximum(array.as_ref().to_owned(), rhs)
        .post(context.solver, None)
        .is_ok())
}

fn compile_array_int_minimum(
    context: &mut CompilationContext<'_>,
    exprs: &[flatzinc::Expr],
) -> Result<bool, FlatZincError> {
    check_parameters!(exprs, 2, "array_int_minimum");

    let rhs = context.resolve_integer_variable(&exprs[0])?;
    let array = context.resolve_integer_variable_array(&exprs[1])?;

    Ok(constraints::minimum(array.as_ref().to_owned(), rhs)
        .post(context.solver, None)
        .is_ok())
}

fn compile_set_in_reif(
    context: &mut CompilationContext<'_>,
    exprs: &[flatzinc::Expr],
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
                .add_clause([
                    !reif.get_true_predicate(),
                    predicate![variable >= lower_bound],
                ])
                .is_ok()
                && context
                    .solver
                    .add_clause([
                        !reif.get_true_predicate(),
                        !predicate![variable >= upper_bound + 1],
                    ])
                    .is_ok();

            // `!reif -> x \notin S`
            // Decomposed to `!reif -> (x < lb \/ x > ub)`
            let backward = context
                .solver
                .add_clause([
                    reif.get_true_predicate(),
                    !predicate![variable >= lower_bound],
                    predicate![variable >= upper_bound + 1],
                ])
                .is_ok();

            forward && backward
        }

        Set::Sparse { values } => {
            let clause = values
                .iter()
                .map(|&value| {
                    context
                        .solver
                        .new_literal_for_predicate(predicate![variable == value])
                })
                .collect::<Vec<_>>();

            constraints::clause(clause)
                .reify(context.solver, reif, None)
                .is_ok()
        }
    };

    Ok(success)
}

fn compile_array_var_int_element(
    context: &mut CompilationContext<'_>,
    exprs: &[flatzinc::Expr],
) -> Result<bool, FlatZincError> {
    check_parameters!(exprs, 3, "array_var_int_element");

    let index = context.resolve_integer_variable(&exprs[0])?.offset(-1);
    let array = context.resolve_integer_variable_array(&exprs[1])?;
    let rhs = context.resolve_integer_variable(&exprs[2])?;

    Ok(constraints::element(index, array.as_ref().to_owned(), rhs)
        .post(context.solver, None)
        .is_ok())
}

fn compile_bool_not(
    context: &mut CompilationContext<'_>,
    exprs: &[flatzinc::Expr],
) -> Result<bool, FlatZincError> {
    // TODO: Take this constraint into account when creating variables, as these can be opposite
    // literals of the same PropositionalVariable. Unsure how often this actually appears in models
    // though.

    check_parameters!(exprs, 2, "bool_not");

    let a = context.resolve_bool_variable(&exprs[0])?;
    let b = context.resolve_bool_variable(&exprs[1])?;

    Ok(constraints::binary_not_equals(a, b)
        .post(context.solver, None)
        .is_ok())
}

fn compile_bool_eq_reif(
    context: &mut CompilationContext<'_>,
    exprs: &[flatzinc::Expr],
) -> Result<bool, FlatZincError> {
    check_parameters!(exprs, 3, "bool_eq_reif");

    let a = context.resolve_bool_variable(&exprs[0])?;
    let b = context.resolve_bool_variable(&exprs[1])?;
    let r = context.resolve_bool_variable(&exprs[2])?;

    Ok(constraints::binary_equals(a, b)
        .reify(context.solver, r, None)
        .is_ok())
}

fn compile_bool_eq(
    context: &mut CompilationContext<'_>,
    exprs: &[flatzinc::Expr],
) -> Result<bool, FlatZincError> {
    // TODO: Take this constraint into account when merging equivalence classes. Unsure how often
    // this actually appears in models though.
    check_parameters!(exprs, 2, "bool_eq");

    let a = context.resolve_bool_variable(&exprs[0])?;
    let b = context.resolve_bool_variable(&exprs[1])?;

    Ok(constraints::binary_equals(a, b)
        .post(context.solver, None)
        .is_ok())
}

fn compile_bool_clause(
    context: &mut CompilationContext<'_>,
    exprs: &[flatzinc::Expr],
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

    Ok(context.solver.add_clause(clause).is_ok())
}

fn compile_bool_and(
    context: &mut CompilationContext<'_>,
    exprs: &[flatzinc::Expr],
) -> Result<bool, FlatZincError> {
    check_parameters!(exprs, 2, "bool_and");

    let a = context.resolve_bool_variable(&exprs[0])?;
    let b = context.resolve_bool_variable(&exprs[1])?;
    let r = context.resolve_bool_variable(&exprs[2])?;

    Ok(constraints::conjunction([a, b])
        .reify(context.solver, r, None)
        .is_ok())
}

fn compile_bool2int(
    context: &mut CompilationContext<'_>,
    exprs: &[flatzinc::Expr],
) -> Result<bool, FlatZincError> {
    // TODO: Perhaps we want to add a phase in the compiler that directly uses the literal
    // corresponding to the predicate [b = 1] for the boolean parameter in this constraint.
    // See https://emir-demirovic.atlassian.net/browse/PUM-89

    check_parameters!(exprs, 2, "bool2int");

    let a = context.resolve_bool_variable(&exprs[0])?;
    let b = context.resolve_integer_variable(&exprs[1])?;

    Ok(
        constraints::binary_equals(a.get_integer_variable(), b.scaled(1))
            .post(context.solver, None)
            .is_ok(),
    )
}

fn compile_bool_or(
    context: &mut CompilationContext<'_>,
    exprs: &[flatzinc::Expr],
) -> Result<bool, FlatZincError> {
    check_parameters!(exprs, 2, "bool_or");

    let clause = context.resolve_bool_variable_array(&exprs[0])?;
    let r = context.resolve_bool_variable(&exprs[1])?;

    Ok(constraints::clause(clause.as_ref())
        .reify(context.solver, r, None)
        .is_ok())
}

fn compile_bool_xor(
    context: &mut CompilationContext<'_>,
    exprs: &[flatzinc::Expr],
) -> Result<bool, FlatZincError> {
    check_parameters!(exprs, 2, "pumpkin_bool_xor");

    let a = context
        .resolve_bool_variable(&exprs[0])?
        .get_true_predicate();
    let b = context
        .resolve_bool_variable(&exprs[1])?
        .get_true_predicate();

    let c1 = context.solver.add_clause([!a, !b]).is_ok();
    let c2 = context.solver.add_clause([b, a]).is_ok();

    Ok(c1 && c2)
}

fn compile_bool_xor_reif(
    context: &mut CompilationContext<'_>,
    exprs: &[flatzinc::Expr],
) -> Result<bool, FlatZincError> {
    check_parameters!(exprs, 3, "pumpkin_bool_xor_reif");

    let a = context.resolve_bool_variable(&exprs[0])?;
    let b = context.resolve_bool_variable(&exprs[1])?;
    let r = context.resolve_bool_variable(&exprs[2])?;

    let c1 = constraints::clause([!a, !b, !r])
        .post(context.solver, None)
        .is_ok();
    let c2 = constraints::clause([!a, b, r])
        .post(context.solver, None)
        .is_ok();
    let c3 = constraints::clause([a, !b, r])
        .post(context.solver, None)
        .is_ok();
    let c4 = constraints::clause([a, b, !r])
        .post(context.solver, None)
        .is_ok();

    Ok(c1 && c2 && c3 && c4)
}

fn compile_array_var_bool_element(
    context: &mut CompilationContext<'_>,
    exprs: &[flatzinc::Expr],
    name: &str,
) -> Result<bool, FlatZincError> {
    check_parameters!(exprs, 3, name);

    let index = context.resolve_integer_variable(&exprs[0])?.offset(-1);
    let array = context.resolve_bool_variable_array(&exprs[1])?;
    let rhs = context.resolve_bool_variable(&exprs[2])?;

    Ok(constraints::element(index, array.iter().cloned(), rhs)
        .post(context.solver, None)
        .is_ok())
}

fn compile_array_bool_and(
    context: &mut CompilationContext<'_>,
    exprs: &[flatzinc::Expr],
) -> Result<bool, FlatZincError> {
    check_parameters!(exprs, 2, "array_bool_and");

    let conjunction = context.resolve_bool_variable_array(&exprs[0])?;
    let r = context.resolve_bool_variable(&exprs[1])?;

    Ok(constraints::conjunction(conjunction.as_ref())
        .reify(context.solver, r, None)
        .is_ok())
}

fn compile_ternary_int_predicate<C: Constraint>(
    context: &mut CompilationContext,
    exprs: &[flatzinc::Expr],
    _: &[flatzinc::Annotation],
    predicate_name: &str,
    create_constraint: impl FnOnce(DomainId, DomainId, DomainId) -> C,
) -> Result<bool, FlatZincError> {
    check_parameters!(exprs, 3, predicate_name);

    let a = context.resolve_integer_variable(&exprs[0])?;
    let b = context.resolve_integer_variable(&exprs[1])?;
    let c = context.resolve_integer_variable(&exprs[2])?;

    let constraint = create_constraint(a, b, c);
    Ok(constraint.post(context.solver, None).is_ok())
}

fn compile_binary_int_predicate<C: Constraint>(
    context: &mut CompilationContext,
    exprs: &[flatzinc::Expr],
    _: &[flatzinc::Annotation],
    predicate_name: &str,
    create_constraint: impl FnOnce(DomainId, DomainId) -> C,
) -> Result<bool, FlatZincError> {
    check_parameters!(exprs, 2, predicate_name);

    let a = context.resolve_integer_variable(&exprs[0])?;
    let b = context.resolve_integer_variable(&exprs[1])?;

    let constraint = create_constraint(a, b);
    Ok(constraint.post(context.solver, None).is_ok())
}

fn compile_reified_binary_int_predicate<C: NegatableConstraint>(
    context: &mut CompilationContext,
    exprs: &[flatzinc::Expr],
    _: &[flatzinc::Annotation],
    predicate_name: &str,
    create_constraint: impl FnOnce(DomainId, DomainId) -> C,
) -> Result<bool, FlatZincError> {
    check_parameters!(exprs, 3, predicate_name);

    let a = context.resolve_integer_variable(&exprs[0])?;
    let b = context.resolve_integer_variable(&exprs[1])?;
    let reif = context.resolve_bool_variable(&exprs[2])?;

    let constraint = create_constraint(a, b);
    Ok(constraint.reify(context.solver, reif, None).is_ok())
}

fn weighted_vars(weights: Rc<[i32]>, vars: Rc<[DomainId]>) -> Box<[AffineView<DomainId>]> {
    vars.iter()
        .zip(weights.iter())
        .map(|(x_i, &w_i)| x_i.scaled(w_i))
        .collect::<Box<[_]>>()
}

fn compile_int_lin_predicate<C: Constraint>(
    context: &mut CompilationContext,
    exprs: &[flatzinc::Expr],
    _: &[flatzinc::Annotation],
    predicate_name: &str,
    create_constraint: impl FnOnce(Box<[AffineView<DomainId>]>, i32) -> C,
) -> Result<bool, FlatZincError> {
    check_parameters!(exprs, 3, predicate_name);

    let weights = context.resolve_array_integer_constants(&exprs[0])?;
    let vars = context.resolve_integer_variable_array(&exprs[1])?;
    let rhs = context.resolve_integer_constant_from_expr(&exprs[2])?;

    let terms = weighted_vars(weights, vars);

    let constraint = create_constraint(terms, rhs);
    Ok(constraint.post(context.solver, None).is_ok())
}

fn compile_reified_int_lin_predicate<C: NegatableConstraint>(
    context: &mut CompilationContext,
    exprs: &[flatzinc::Expr],
    _: &[flatzinc::Annotation],
    predicate_name: &str,
    create_constraint: impl FnOnce(Box<[AffineView<DomainId>]>, i32) -> C,
) -> Result<bool, FlatZincError> {
    check_parameters!(exprs, 4, predicate_name);

    let weights = context.resolve_array_integer_constants(&exprs[0])?;
    let vars = context.resolve_integer_variable_array(&exprs[1])?;
    let rhs = context.resolve_integer_constant_from_expr(&exprs[2])?;
    let reif = context.resolve_bool_variable(&exprs[3])?;

    let terms = weighted_vars(weights, vars);

    let constraint = create_constraint(terms, rhs);
    Ok(constraint.reify(context.solver, reif, None).is_ok())
}

fn compile_bool_lin_eq_predicate(
    context: &mut CompilationContext,
    exprs: &[flatzinc::Expr],
) -> Result<bool, FlatZincError> {
    check_parameters!(exprs, 3, "bool_lin_eq");

    let weights = context.resolve_array_integer_constants(&exprs[0])?;
    let bools = context.resolve_bool_variable_array(&exprs[1])?;
    let rhs = context.resolve_integer_variable(&exprs[2])?;

    Ok(
        constraints::boolean_equals(weights.as_ref().to_owned(), bools.as_ref().to_owned(), rhs)
            .post(context.solver, None)
            .is_ok(),
    )
}

fn compile_bool_lin_le_predicate(
    context: &mut CompilationContext,
    exprs: &[flatzinc::Expr],
) -> Result<bool, FlatZincError> {
    check_parameters!(exprs, 3, "bool_lin_le");

    let weights = context.resolve_array_integer_constants(&exprs[0])?;
    let bools = context.resolve_bool_variable_array(&exprs[1])?;
    let rhs = context.resolve_integer_constant_from_expr(&exprs[2])?;

    Ok(constraints::boolean_less_than_or_equals(
        weights.as_ref().to_owned(),
        bools.as_ref().to_owned(),
        rhs,
    )
    .post(context.solver, None)
    .is_ok())
}

fn compile_all_different(
    context: &mut CompilationContext,
    exprs: &[flatzinc::Expr],
    _: &[flatzinc::Annotation],
) -> Result<bool, FlatZincError> {
    check_parameters!(exprs, 1, "fzn_all_different");

    let variables = context.resolve_integer_variable_array(&exprs[0])?.to_vec();
    Ok(constraints::all_different(variables)
        .post(context.solver, None)
        .is_ok())
}
