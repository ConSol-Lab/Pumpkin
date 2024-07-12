//! Compile constraints into CP propagators

use std::rc::Rc;

use pumpkin_lib::constraints::ConstraintsExt;
use pumpkin_lib::engine::variables::AffineView;
use pumpkin_lib::engine::variables::DomainId;
use pumpkin_lib::engine::variables::Literal;
use pumpkin_lib::engine::variables::TransformableVariable;
use pumpkin_lib::engine::ConstraintSatisfactionSolver;

use super::constraints::bool_lin_eq;
use super::constraints::bool_lin_le;
use super::constraints::int_eq_reif;
use super::constraints::int_le_reif;
use super::constraints::int_lin_eq_reif;
use super::constraints::int_lin_le_reif;
use super::constraints::int_lin_ne_reif;
use super::constraints::int_lt_reif;
use super::constraints::int_ne_reif;
use super::context::CompilationContext;
use crate::flatzinc::ast::FlatZincAst;
use crate::flatzinc::compiler::constraints::array_bool_or;
use crate::flatzinc::FlatZincError;
use crate::flatzinc::FlatZincOptions;

pub(crate) fn run(
    ast: &FlatZincAst,
    context: &mut CompilationContext,
    options: FlatZincOptions,
) -> Result<(), FlatZincError> {
    for constraint_item in &ast.constraint_decls {
        let flatzinc::ConstraintItem { id, exprs, annos } = constraint_item;

        let is_satisfiable = match id.as_str() {
            "array_int_maximum" => compile_array_int_maximum(context, exprs)?,
            "array_int_minimum" => compile_array_int_minimum(context, exprs)?,

            // We rewrite `array_int_element` to `array_var_int_element`.
            "array_int_element" => compile_array_var_int_element(context, exprs)?,
            "array_var_int_element" => compile_array_var_int_element(context, exprs)?,

            "int_lin_ne" => compile_int_lin_predicate(
                context,
                exprs,
                annos,
                "int_lin_ne",
                |solver, terms, rhs| solver.int_lin_ne(terms, rhs),
            )?,
            "int_lin_ne_reif" => compile_reified_int_lin_predicate(
                context,
                exprs,
                annos,
                "int_lin_ne_reif",
                int_lin_ne_reif,
            )?,
            "int_lin_le" => compile_int_lin_predicate(
                context,
                exprs,
                annos,
                "int_lin_le",
                |solver, terms, rhs| solver.int_lin_le(terms, rhs),
            )?,
            "int_lin_le_reif" => compile_reified_int_lin_predicate(
                context,
                exprs,
                annos,
                "int_lin_le_reif",
                int_lin_le_reif,
            )?,
            "int_lin_eq" => compile_int_lin_predicate(
                context,
                exprs,
                annos,
                "int_lin_eq",
                |solver, terms, rhs| solver.int_lin_eq(terms, rhs),
            )?,
            "int_lin_eq_reif" => compile_reified_int_lin_predicate(
                context,
                exprs,
                annos,
                "int_lin_eq_reif",
                int_lin_eq_reif,
            )?,
            "int_ne" => {
                compile_binary_int_predicate(context, exprs, annos, "int_ne", |solver, a, b| {
                    solver.int_ne(a, b)
                })?
            }
            "int_ne_reif" => compile_reified_binary_int_predicate(
                context,
                exprs,
                annos,
                "int_ne_reif",
                int_ne_reif,
            )?,
            "int_le" => {
                compile_binary_int_predicate(context, exprs, annos, "int_le", |solver, a, b| {
                    solver.int_le(a, b)
                })?
            }
            "int_le_reif" => compile_reified_binary_int_predicate(
                context,
                exprs,
                annos,
                "int_le_reif",
                int_le_reif,
            )?,
            "int_lt" => {
                compile_binary_int_predicate(context, exprs, annos, "int_lt", |solver, a, b| {
                    solver.int_lt(a, b)
                })?
            }
            "int_lt_reif" => compile_reified_binary_int_predicate(
                context,
                exprs,
                annos,
                "int_lt_reif",
                int_lt_reif,
            )?,
            "int_eq" => {
                compile_binary_int_predicate(context, exprs, annos, "int_eq", |solver, a, b| {
                    solver.int_eq(a, b)
                })?
            }
            "int_eq_reif" => compile_reified_binary_int_predicate(
                context,
                exprs,
                annos,
                "int_eq_reif",
                int_eq_reif,
            )?,
            "int_plus" => compile_ternary_int_predicate(context, exprs, annos, "int_plus", |solver, a, b, c| {
                solver.int_plus(a, b, c)
            })?,
            "int_times" => compile_ternary_int_predicate(context, exprs, annos, "int_times", |solver, a, b, c| {
                solver.int_times(a, b, c)
            })?,
            "int_div" => compile_ternary_int_predicate(context, exprs, annos, "int_div", |solver, a, b, c| {
                solver.int_div(a, b, c)
            })?,
            "int_abs" => {
                compile_binary_int_predicate(context, exprs, annos, "int_abs", |solver, a, b| {
                    solver.int_abs(a, b)
                })?
            }
            "int_max" => compile_int_max(context, exprs)?,
            "int_min" => compile_int_min(context, exprs)?,
            "fzn_all_different_int" => compile_all_different(context, exprs, annos)?,

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
                // We do not do anything further as we handle the domain changes in a pre-processing step (see `handle_set_in.rs`).
                true
            },

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

    Ok(context.solver.cumulative(
        &start_times,
        &durations,
        &resource_requirements,
        resource_capacity,
        options.cumulative_allow_holes,
    ))
}

fn compile_array_int_maximum(
    context: &mut CompilationContext<'_>,
    exprs: &[flatzinc::Expr],
) -> Result<bool, FlatZincError> {
    check_parameters!(exprs, 2, "array_int_maximum");

    let rhs = context.resolve_integer_variable(&exprs[0])?;
    let array = context.resolve_integer_variable_array(&exprs[1])?;

    Ok(context.solver.maximum(array.as_ref(), rhs))
}

fn compile_array_int_minimum(
    context: &mut CompilationContext<'_>,
    exprs: &[flatzinc::Expr],
) -> Result<bool, FlatZincError> {
    check_parameters!(exprs, 2, "array_int_minimum");

    let rhs = context.resolve_integer_variable(&exprs[0])?;
    let array = context.resolve_integer_variable_array(&exprs[1])?;

    Ok(context.solver.minimum(array.iter().copied(), rhs))
}

fn compile_int_max(
    context: &mut CompilationContext<'_>,
    exprs: &[flatzinc::Expr],
) -> Result<bool, FlatZincError> {
    check_parameters!(exprs, 3, "int_max");

    let a = context.resolve_integer_variable(&exprs[0])?;
    let b = context.resolve_integer_variable(&exprs[1])?;
    let c = context.resolve_integer_variable(&exprs[2])?;

    Ok(context.solver.maximum([a, b], c))
}

fn compile_int_min(
    context: &mut CompilationContext<'_>,
    exprs: &[flatzinc::Expr],
) -> Result<bool, FlatZincError> {
    check_parameters!(exprs, 3, "int_min");

    let a = context.resolve_integer_variable(&exprs[0])?;
    let b = context.resolve_integer_variable(&exprs[1])?;
    let c = context.resolve_integer_variable(&exprs[2])?;

    Ok(context.solver.minimum([a, b], c))
}

fn compile_set_in_reif(
    _context: &mut CompilationContext<'_>,
    _exprs: &[flatzinc::Expr],
) -> Result<bool, FlatZincError> {
    todo!();
    // check_parameters!(exprs, 3, "set_in_reif");
    //
    // let variable = context.resolve_integer_variable(&exprs[0])?;
    // let set = context.resolve_set_constant(&exprs[1])?;
    // let reif = context.resolve_bool_variable(&exprs[2])?;
    //
    // let success = match set {
    // Set::Interval {
    // lower_bound,
    // upper_bound,
    // } => {
    // `reif -> x \in S`
    // Decomposed to `reif -> x >= lb /\ reif -> x <= ub`
    // let forward = context
    // .solver
    // .add_clause([
    // !reif,
    // context
    // .solver
    // .get_literal(predicate![variable >= lower_bound]),
    // ])
    // .is_ok()
    // && context
    // .solver
    // .add_clause([
    // !reif,
    // !context
    // .solver
    // .get_literal(predicate![variable >= upper_bound + 1]),
    // ])
    // .is_ok();
    //
    // `!reif -> x \notin S`
    // Decomposed to `!reif -> (x < lb \/ x > ub)`
    // let backward = context
    // .solver
    // .add_clause([
    // reif,
    // !context
    // .solver
    // .get_literal(predicate![variable >= lower_bound]),
    // context
    // .solver
    // .get_literal(predicate![variable >= upper_bound + 1]),
    // ])
    // .is_ok();
    //
    // forward && backward
    // }
    //
    // Set::Sparse { values } => {
    // let clause = values
    // .iter()
    // .map(|&value| predicate![variable == value])
    // .collect::<Vec<_>>();
    //
    // array_bool_or(context.solver, clause, reif)
    // }
    // };
    //
    // Ok(success)
}

fn compile_array_var_int_element(
    context: &mut CompilationContext<'_>,
    exprs: &[flatzinc::Expr],
) -> Result<bool, FlatZincError> {
    check_parameters!(exprs, 3, "array_var_int_element");

    let index = context.resolve_integer_variable(&exprs[0])?.offset(-1);
    let array = context.resolve_integer_variable_array(&exprs[1])?;
    let rhs = context.resolve_integer_variable(&exprs[2])?;

    Ok(context
        .solver
        .array_var_int_element(index, array.as_ref(), rhs))
}

fn compile_bool_not(
    _context: &mut CompilationContext<'_>,
    _exprs: &[flatzinc::Expr],
) -> Result<bool, FlatZincError> {
    todo!();

    // TODO: Take this constraint into account when creating variables, as these can be opposite
    // literals of the same PropositionalVariable. Unsure how often this actually appears in models
    // though.
    // check_parameters!(exprs, 2, "bool_not");
    //
    // let a = context.resolve_bool_variable(&exprs[0])?;
    // let b = context.resolve_bool_variable(&exprs[1])?;
    //
    // a != b
    // -> !(a /\ b) /\ !(!a /\ !b)
    // -> (!a \/ !b) /\ (a \/ b)
    // let c1 = context.solver.add_clause([a, b]).is_ok();
    // let c2 = context.solver.add_clause([!a, !b]).is_ok();
    //
    // Ok(c1 && c2)
}

fn compile_bool_eq_reif(
    _context: &mut CompilationContext<'_>,
    _exprs: &[flatzinc::Expr],
) -> Result<bool, FlatZincError> {
    todo!();
    // check_parameters!(exprs, 3, "bool_eq_reif");
    //
    // let a = context.resolve_bool_variable(&exprs[0])?;
    // let b = context.resolve_bool_variable(&exprs[1])?;
    // let r = context.resolve_bool_variable(&exprs[2])?;
    //
    // let c1 = context.solver.add_clause([!a, !b, r]).is_ok();
    // let c2 = context.solver.add_clause([!a, b, !r]).is_ok();
    // let c3 = context.solver.add_clause([a, !b, !r]).is_ok();
    // let c4 = context.solver.add_clause([a, b, r]).is_ok();
    //
    // Ok(c1 && c2 && c3 && c4)
}

fn compile_bool_eq(
    _context: &mut CompilationContext<'_>,
    _exprs: &[flatzinc::Expr],
) -> Result<bool, FlatZincError> {
    todo!();
    // TODO: Take this constraint into account when merging equivalence classes. Unsure how often
    // this actually appears in models though.
    // check_parameters!(exprs, 2, "bool_eq");
    //
    // let a = context.resolve_bool_variable(&exprs[0])?;
    // let b = context.resolve_bool_variable(&exprs[1])?;
    //
    // let c1 = context.solver.add_clause([!a, b]).is_ok();
    // let c2 = context.solver.add_clause([!b, a]).is_ok();
    //
    // Ok(c1 && c2)
}

fn compile_bool_clause(
    _context: &mut CompilationContext<'_>,
    _exprs: &[flatzinc::Expr],
) -> Result<bool, FlatZincError> {
    todo!();
    // check_parameters!(exprs, 2, "bool_clause");
    //
    // let clause_1 = context.resolve_bool_variable_array(&exprs[0])?;
    // let clause_2 = context.resolve_bool_variable_array(&exprs[1])?;
    //
    // let clause: Vec<IntegerPredicate> = clause_1
    // .iter()
    // .copied()
    // .chain(clause_2.iter().map(|&literal| !literal))
    // .collect();
    // Ok(context.solver.add_clause(clause).is_ok())
}

fn compile_bool_and(
    _context: &mut CompilationContext<'_>,
    _exprs: &[flatzinc::Expr],
) -> Result<bool, FlatZincError> {
    todo!();
    // check_parameters!(exprs, 2, "bool_and");
    //
    // let a = context.resolve_bool_variable(&exprs[0])?;
    // let b = context.resolve_bool_variable(&exprs[1])?;
    // let r = context.resolve_bool_variable(&exprs[2])?;
    //
    // let c1 = context.solver.add_clause([!r, a]).is_ok();
    // let c2 = context.solver.add_clause([!r, b]).is_ok();
    //
    // let c3 = context.solver.add_clause([!a, !b, r]).is_ok();
    //
    // Ok(c1 && c2 && c3)
}

fn compile_bool2int(
    _context: &mut CompilationContext<'_>,
    _exprs: &[flatzinc::Expr],
) -> Result<bool, FlatZincError> {
    todo!();
    // TODO: Perhaps we want to add a phase in the compiler that directly uses the literal
    // corresponding to the predicate [b = 1] for the boolean parameter in this constraint.
    // See https://emir-demirovic.atlassian.net/browse/PUM-89
    // check_parameters!(exprs, 2, "bool2int");
    //
    // let a = context.resolve_bool_variable(&exprs[0])?;
    // let b = context.resolve_integer_variable(&exprs[1])?;
    //
    // let b_lit = predicate![b == 1];
    //
    // let c1 = context.solver.add_clause([!a, b_lit]).is_ok();
    // let c2 = context.solver.add_clause([!b_lit, a]).is_ok();
    //
    // Ok(c1 && c2)
}

fn compile_bool_or(
    context: &mut CompilationContext<'_>,
    exprs: &[flatzinc::Expr],
) -> Result<bool, FlatZincError> {
    check_parameters!(exprs, 2, "bool_or");

    let clause = context.resolve_bool_variable_array(&exprs[0])?;
    let r = context.resolve_bool_variable(&exprs[1])?;

    Ok(array_bool_or(context.solver, clause.as_ref(), r))
}

fn compile_bool_xor(
    _context: &mut CompilationContext<'_>,
    _exprs: &[flatzinc::Expr],
) -> Result<bool, FlatZincError> {
    todo!();
    // check_parameters!(exprs, 2, "pumpkin_bool_xor");
    //
    // let a = context.resolve_bool_variable(&exprs[0])?;
    // let b = context.resolve_bool_variable(&exprs[1])?;
    //
    // let c1 = context.solver.add_clause([!a, !b]).is_ok();
    // let c2 = context.solver.add_clause([b, a]).is_ok();
    //
    // Ok(c1 && c2)
}

fn compile_bool_xor_reif(
    _context: &mut CompilationContext<'_>,
    _exprs: &[flatzinc::Expr],
) -> Result<bool, FlatZincError> {
    todo!();
    // check_parameters!(exprs, 3, "pumpkin_bool_xor_reif");
    //
    // let a = context.resolve_bool_variable(&exprs[0])?;
    // let b = context.resolve_bool_variable(&exprs[1])?;
    // let r = context.resolve_bool_variable(&exprs[2])?;
    //
    // let c1 = context.solver.add_clause([!a, !b, !r]).is_ok();
    // let c2 = context.solver.add_clause([!a, b, r]).is_ok();
    // let c3 = context.solver.add_clause([a, !b, r]).is_ok();
    // let c4 = context.solver.add_clause([a, b, !r]).is_ok();
    //
    // Ok(c1 && c2 && c3 && c4)
}

fn compile_array_var_bool_element(
    _context: &mut CompilationContext<'_>,
    _exprs: &[flatzinc::Expr],
    _name: &str,
) -> Result<bool, FlatZincError> {
    todo!();
    // check_parameters!(exprs, 3, name);
    //
    // let index = context.resolve_integer_variable(&exprs[0])?;
    // let array = context.resolve_bool_variable_array(&exprs[1])?;
    // let rhs = context.resolve_bool_variable(&exprs[2])?;
    //
    // let mut success = true;
    //
    // for i in 0..array.len() {
    // Note: minizinc arrays are 1-indexed.
    // let mzn_index = i as i32 + 1;
    //
    // [index = mzn_index] -> (rhs <-> array[i])
    //
    // let predicate_lit = predicate![index == mzn_index];
    //
    // success &= context
    // .solver
    // .add_clause([!predicate_lit, !rhs, array[i]])
    // .is_ok();
    // success &= context
    // .solver
    // .add_clause([!predicate_lit, !array[i], rhs])
    // .is_ok();
    // }
    //
    // Ok(success)
}

fn compile_array_bool_and(
    _context: &mut CompilationContext<'_>,
    _exprs: &[flatzinc::Expr],
) -> Result<bool, FlatZincError> {
    todo!();
    // check_parameters!(exprs, 2, "array_bool_and");
    //
    // let conjunction = context.resolve_bool_variable_array(&exprs[0])?;
    // let r = context.resolve_bool_variable(&exprs[1])?;
    //
    // /\conjunction -> r
    // let clause: Vec<IntegerPredicate> = conjunction
    // .iter()
    // .map(|&literal| !literal)
    // .chain(std::iter::once(r))
    // .collect();
    // let first_implication = context.solver.add_clause(clause).is_ok();
    //
    // r -> /\conjunction
    // let second_implication = conjunction
    // .iter()
    // .all(|&literal| context.solver.add_clause([!r, literal]).is_ok());
    //
    // Ok(first_implication && second_implication)
}

fn compile_ternary_int_predicate(
    context: &mut CompilationContext,
    exprs: &[flatzinc::Expr],
    _: &[flatzinc::Annotation],
    predicate_name: &str,
    post_constraint: impl FnOnce(
        &mut ConstraintSatisfactionSolver,
        DomainId,
        DomainId,
        DomainId,
    ) -> bool,
) -> Result<bool, FlatZincError> {
    check_parameters!(exprs, 3, predicate_name);

    let a = context.resolve_integer_variable(&exprs[0])?;
    let b = context.resolve_integer_variable(&exprs[1])?;
    let c = context.resolve_integer_variable(&exprs[2])?;

    Ok(post_constraint(context.solver, a, b, c))
}

fn compile_binary_int_predicate(
    context: &mut CompilationContext,
    exprs: &[flatzinc::Expr],
    _: &[flatzinc::Annotation],
    predicate_name: &str,
    post_constraint: impl FnOnce(&mut ConstraintSatisfactionSolver, DomainId, DomainId) -> bool,
) -> Result<bool, FlatZincError> {
    check_parameters!(exprs, 2, predicate_name);

    let a = context.resolve_integer_variable(&exprs[0])?;
    let b = context.resolve_integer_variable(&exprs[1])?;

    Ok(post_constraint(context.solver, a, b))
}

fn compile_reified_binary_int_predicate(
    context: &mut CompilationContext,
    exprs: &[flatzinc::Expr],
    _: &[flatzinc::Annotation],
    predicate_name: &str,
    post_constraint: impl FnOnce(&mut ConstraintSatisfactionSolver, DomainId, DomainId, Literal) -> bool,
) -> Result<bool, FlatZincError> {
    check_parameters!(exprs, 3, predicate_name);

    let a = context.resolve_integer_variable(&exprs[0])?;
    let b = context.resolve_integer_variable(&exprs[1])?;
    let reif = context.resolve_bool_variable(&exprs[2])?;

    Ok(post_constraint(context.solver, a, b, reif))
}

fn weighted_vars(weights: Rc<[i32]>, vars: Rc<[DomainId]>) -> Box<[AffineView<DomainId>]> {
    vars.iter()
        .zip(weights.iter())
        .map(|(x_i, &w_i)| x_i.scaled(w_i))
        .collect::<Box<[_]>>()
}

fn compile_int_lin_predicate(
    context: &mut CompilationContext,
    exprs: &[flatzinc::Expr],
    _: &[flatzinc::Annotation],
    predicate_name: &str,
    post_constraint: impl FnOnce(
        &mut ConstraintSatisfactionSolver,
        Box<[AffineView<DomainId>]>,
        i32,
    ) -> bool,
) -> Result<bool, FlatZincError> {
    check_parameters!(exprs, 3, predicate_name);

    let weights = context.resolve_array_integer_constants(&exprs[0])?;
    let vars = context.resolve_integer_variable_array(&exprs[1])?;
    let rhs = context.resolve_integer_constant_from_expr(&exprs[2])?;

    let terms = weighted_vars(weights, vars);

    Ok(post_constraint(context.solver, terms, rhs))
}

fn compile_reified_int_lin_predicate(
    context: &mut CompilationContext,
    exprs: &[flatzinc::Expr],
    _: &[flatzinc::Annotation],
    predicate_name: &str,
    post_constraint: impl FnOnce(
        &mut ConstraintSatisfactionSolver,
        Box<[AffineView<DomainId>]>,
        i32,
        Literal,
    ) -> bool,
) -> Result<bool, FlatZincError> {
    check_parameters!(exprs, 4, predicate_name);

    let weights = context.resolve_array_integer_constants(&exprs[0])?;
    let vars = context.resolve_integer_variable_array(&exprs[1])?;
    let rhs = context.resolve_integer_constant_from_expr(&exprs[2])?;
    let reif = context.resolve_bool_variable(&exprs[3])?;

    let terms = weighted_vars(weights, vars);

    Ok(post_constraint(context.solver, terms, rhs, reif))
}

fn compile_bool_lin_eq_predicate(
    context: &mut CompilationContext,
    exprs: &[flatzinc::Expr],
) -> Result<bool, FlatZincError> {
    check_parameters!(exprs, 3, "bool_lin_eq");

    let weights = context.resolve_array_integer_constants(&exprs[0])?;
    let bools = context.resolve_bool_variable_array(&exprs[1])?;
    let rhs = context.resolve_integer_variable(&exprs[2])?;

    Ok(bool_lin_eq(context.solver, &weights, &bools, rhs))
}

fn compile_bool_lin_le_predicate(
    context: &mut CompilationContext,
    exprs: &[flatzinc::Expr],
) -> Result<bool, FlatZincError> {
    check_parameters!(exprs, 3, "bool_lin_le");

    let weights = context.resolve_array_integer_constants(&exprs[0])?;
    let bools = context.resolve_bool_variable_array(&exprs[1])?;
    let rhs = context.resolve_integer_constant_from_expr(&exprs[2])?;

    Ok(bool_lin_le(context.solver, &weights, &bools, rhs))
}

fn compile_all_different(
    context: &mut CompilationContext,
    exprs: &[flatzinc::Expr],
    _: &[flatzinc::Annotation],
) -> Result<bool, FlatZincError> {
    check_parameters!(exprs, 1, "fzn_all_different");

    let variables = context.resolve_integer_variable_array(&exprs[0])?.to_vec();
    Ok(context.solver.all_different(variables))
}
