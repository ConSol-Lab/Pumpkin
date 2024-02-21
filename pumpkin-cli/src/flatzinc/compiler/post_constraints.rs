//! Compile constraints into CP propagators

use std::rc::Rc;

use pumpkin_lib::basic_types::variables::AffineView;
use pumpkin_lib::basic_types::variables::IntVar;
use pumpkin_lib::basic_types::DomainId;
use pumpkin_lib::basic_types::Literal;
use pumpkin_lib::constraints::ConstraintsExt;
use pumpkin_lib::engine::ConstraintSatisfactionSolver;
use pumpkin_lib::predicate;

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
use crate::flatzinc::compiler::context::Set;
use crate::flatzinc::FlatZincError;

pub(crate) fn run(
    ast: &FlatZincAst,
    context: &mut CompilationContext,
) -> Result<(), FlatZincError> {
    for constraint_item in &ast.constraint_decls {
        let flatzinc::ConstraintItem { id, exprs, annos } = constraint_item;

        match id.as_str() {
            // We rewrite `array_int_element` to `array_var_int_element`.
            "array_int_element" => compile_array_var_int_element(context, exprs)?,
            "array_var_int_element" => compile_array_var_int_element(context, exprs)?,
            "int_lin_ne" => {
                compile_int_lin_predicate(
                    context,
                    exprs,
                    annos,
                    "int_lin_ne",
                    |solver, terms, rhs| solver.int_lin_ne(terms, rhs),
                )?;
            }
            "int_lin_ne_reif" => compile_reified_int_lin_predicate(
                context,
                exprs,
                annos,
                "int_lin_ne_reif",
                int_lin_ne_reif,
            )?,
            "int_lin_le" => {
                compile_int_lin_predicate(
                    context,
                    exprs,
                    annos,
                    "int_lin_le",
                    |solver, terms, rhs| solver.int_lin_le(terms, rhs),
                )?;
            }
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
            "int_plus" => compile_int_plus(context, exprs, annos)?,
            "int_times" => compile_int_times(context, exprs, annos)?,
            "fzn_all_different_int" => compile_all_different(context, exprs, annos)?,

            "array_bool_and" => compile_array_bool_and(context, exprs)?,
            "array_var_bool_element" => compile_array_var_bool_element(context, exprs)?,
            "array_bool_or" => compile_bool_or(context, exprs)?,
            "array_bool_xor" => todo!("implement support for array_bool_xor"),

            "bool2int" => compile_bool2int(context, exprs)?,

            "bool_and" => compile_bool_and(context, exprs)?,
            "bool_clause" => compile_bool_clause(context, exprs)?,
            "bool_eq" => compile_bool_eq(context, exprs)?,
            "bool_eq_reif" => compile_bool_eq_reif(context, exprs)?,
            "bool_not" => compile_bool_not(context, exprs)?,

            "par_set_in_reif" => compile_set_in_reif(context, exprs)?,

            unknown => todo!("unsupported constraint {unknown}"),
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

fn compile_set_in_reif(
    context: &mut CompilationContext<'_>,
    exprs: &[flatzinc::Expr],
) -> Result<(), FlatZincError> {
    check_parameters!(exprs, 3, "par_set_in_reif");

    let variable = context.resolve_integer_variable(&exprs[0])?;
    let set = context.resolve_set_constant(&exprs[1])?;
    let reif = context.resolve_bool_variable(&exprs[2])?;

    match set {
        Set::Interval {
            lower_bound,
            upper_bound,
        } => {
            let lb_variable = *context
                .constant_domain_ids
                .entry(lower_bound)
                .or_insert_with(|| {
                    context
                        .solver
                        .create_new_integer_variable(lower_bound, lower_bound)
                });
            let ub_variable = *context
                .constant_domain_ids
                .entry(upper_bound)
                .or_insert_with(|| {
                    context
                        .solver
                        .create_new_integer_variable(upper_bound, upper_bound)
                });

            int_le_reif(context.solver, variable, ub_variable, reif);
            int_le_reif(context.solver, lb_variable, variable, reif);
        }

        Set::Sparse { values } => {
            let clause = values
                .iter()
                .map(|&value| context.solver.get_literal(predicate![variable == value]))
                .collect::<Vec<_>>();

            array_bool_or(context.solver, clause, reif);
        }
    }

    Ok(())
}

fn compile_array_var_int_element(
    context: &mut CompilationContext<'_>,
    exprs: &[flatzinc::Expr],
) -> Result<(), FlatZincError> {
    check_parameters!(exprs, 3, "array_var_int_element");

    let index = context.resolve_integer_variable(&exprs[0])?.offset(-1);
    let array = context.resolve_integer_variable_array(&exprs[1])?;
    let rhs = context.resolve_integer_variable(&exprs[2])?;

    context
        .solver
        .array_var_int_element(index, array.as_ref(), rhs);

    Ok(())
}

fn compile_bool_not(
    context: &mut CompilationContext<'_>,
    exprs: &[flatzinc::Expr],
) -> Result<(), FlatZincError> {
    // TODO: Take this constraint into account when creating variables, as these can be opposite
    // literals of the same PropositionalVariable. Unsure how often this actually appears in models
    // though.
    check_parameters!(exprs, 2, "bool_eq");

    let a = context.resolve_bool_variable(&exprs[0])?;
    let b = context.resolve_bool_variable(&exprs[1])?;

    let _ = context.solver.add_permanent_clause(vec![!a, !b]);
    let _ = context.solver.add_permanent_clause(vec![!b, !a]);

    Ok(())
}

fn compile_bool_eq_reif(
    context: &mut CompilationContext<'_>,
    exprs: &[flatzinc::Expr],
) -> Result<(), FlatZincError> {
    check_parameters!(exprs, 3, "bool_eq_reif");

    let a = context.resolve_bool_variable(&exprs[0])?;
    let b = context.resolve_bool_variable(&exprs[1])?;
    let r = context.resolve_bool_variable(&exprs[2])?;

    let _ = context.solver.add_permanent_clause(vec![!a, !b, r]);
    let _ = context.solver.add_permanent_clause(vec![!a, b, !r]);
    let _ = context.solver.add_permanent_clause(vec![a, !b, !r]);
    let _ = context.solver.add_permanent_clause(vec![a, b, r]);

    Ok(())
}

fn compile_bool_eq(
    context: &mut CompilationContext<'_>,
    exprs: &[flatzinc::Expr],
) -> Result<(), FlatZincError> {
    // TODO: Take this constraint into account when merging equivalence classes. Unsure how often
    // this actually appears in models though.
    check_parameters!(exprs, 2, "bool_eq");

    let a = context.resolve_bool_variable(&exprs[0])?;
    let b = context.resolve_bool_variable(&exprs[1])?;

    let _ = context.solver.add_permanent_clause(vec![!a, b]);
    let _ = context.solver.add_permanent_clause(vec![!b, a]);

    Ok(())
}

fn compile_bool_clause(
    context: &mut CompilationContext<'_>,
    exprs: &[flatzinc::Expr],
) -> Result<(), FlatZincError> {
    check_parameters!(exprs, 2, "bool_clause");

    let clause_1 = context.resolve_bool_variable_array(&exprs[0])?;
    let clause_2 = context.resolve_bool_variable_array(&exprs[1])?;

    let clause = clause_1
        .iter()
        .copied()
        .chain(clause_2.iter().map(|&literal| !literal))
        .collect();
    let _ = context.solver.add_permanent_clause(clause);

    Ok(())
}

fn compile_bool_and(
    context: &mut CompilationContext<'_>,
    exprs: &[flatzinc::Expr],
) -> Result<(), FlatZincError> {
    check_parameters!(exprs, 2, "bool_and");

    let a = context.resolve_bool_variable(&exprs[0])?;
    let b = context.resolve_bool_variable(&exprs[1])?;
    let r = context.resolve_bool_variable(&exprs[2])?;

    let _ = context.solver.add_permanent_clause(vec![!r, a]);
    let _ = context.solver.add_permanent_clause(vec![!r, b]);

    let _ = context.solver.add_permanent_clause(vec![!a, !b, r]);

    Ok(())
}

fn compile_bool2int(
    context: &mut CompilationContext<'_>,
    exprs: &[flatzinc::Expr],
) -> Result<(), FlatZincError> {
    // TODO: Perhaps we want to add a phase in the compiler that directly uses the literal
    // corresponding to the predicate [b = 1] for the boolean parameter in this constraint.
    // See https://emir-demirovic.atlassian.net/jira/software/projects/PUM/boards/1?selectedIssue=PUM-89
    check_parameters!(exprs, 2, "bool2int");

    let a = context.resolve_bool_variable(&exprs[0])?;
    let b = context.resolve_integer_variable(&exprs[1])?;

    let b_lit = context.solver.get_literal(predicate![b == 1]);

    let _ = context.solver.add_permanent_clause(vec![!a, b_lit]);
    let _ = context.solver.add_permanent_clause(vec![!b_lit, a]);

    Ok(())
}

fn compile_bool_or(
    context: &mut CompilationContext<'_>,
    exprs: &[flatzinc::Expr],
) -> Result<(), FlatZincError> {
    check_parameters!(exprs, 2, "bool_or");

    let clause = context.resolve_bool_variable_array(&exprs[0])?;
    let r = context.resolve_bool_variable(&exprs[1])?;

    array_bool_or(context.solver, clause.as_ref(), r);

    Ok(())
}

fn compile_array_var_bool_element(
    context: &mut CompilationContext<'_>,
    exprs: &[flatzinc::Expr],
) -> Result<(), FlatZincError> {
    check_parameters!(exprs, 3, "array_bool_element");

    let index = context.resolve_integer_variable(&exprs[0])?;
    let array = context.resolve_bool_variable_array(&exprs[1])?;
    let rhs = context.resolve_bool_variable(&exprs[2])?;

    for i in 0..array.len() {
        // rhs <-> [index = i] /\ array[i]

        let value = i as i32;
        let predicate_lit = context.solver.get_literal(predicate![index == value]);

        // rhs <- [index = i] /\ array[i]
        let _ = context
            .solver
            .add_permanent_clause(vec![!predicate_lit, !array[i], rhs]);

        // rhs -> [index = i] /\ array[i]
        let _ = context
            .solver
            .add_permanent_clause(vec![!rhs, predicate_lit]);
        let _ = context.solver.add_permanent_clause(vec![!rhs, array[i]]);
    }

    Ok(())
}

fn compile_array_bool_and(
    context: &mut CompilationContext<'_>,
    exprs: &[flatzinc::Expr],
) -> Result<(), FlatZincError> {
    check_parameters!(exprs, 2, "array_bool_and");

    let conjunction = context.resolve_bool_variable_array(&exprs[0])?;
    let r = context.resolve_bool_variable(&exprs[1])?;

    // /\conjunction -> r
    let clause = conjunction
        .iter()
        .map(|&literal| !literal)
        .chain(std::iter::once(r))
        .collect();
    let _ = context.solver.add_permanent_clause(clause);

    // r -> /\conjunction
    conjunction.iter().for_each(|&literal| {
        let _ = context.solver.add_permanent_clause(vec![!r, literal]);
    });

    Ok(())
}

fn compile_int_plus(
    context: &mut CompilationContext,
    exprs: &[flatzinc::Expr],
    _: &[flatzinc::Annotation],
) -> Result<(), FlatZincError> {
    check_parameters!(exprs, 3, "int_plus");

    let a = context.resolve_integer_variable(&exprs[0])?;
    let b = context.resolve_integer_variable(&exprs[1])?;
    let c = context.resolve_integer_variable(&exprs[2])?;

    context.solver.int_plus(a, b, c);

    Ok(())
}

fn compile_int_times(
    context: &mut CompilationContext,
    exprs: &[flatzinc::Expr],
    _: &[flatzinc::Annotation],
) -> Result<(), FlatZincError> {
    check_parameters!(exprs, 3, "int_times");

    let a = context.resolve_integer_variable(&exprs[0])?;
    let b = context.resolve_integer_variable(&exprs[1])?;
    let c = context.resolve_integer_variable(&exprs[2])?;

    context.solver.int_times(a, b, c);

    Ok(())
}

fn compile_binary_int_predicate(
    context: &mut CompilationContext,
    exprs: &[flatzinc::Expr],
    _: &[flatzinc::Annotation],
    predicate_name: &str,
    post_constraint: impl FnOnce(&mut ConstraintSatisfactionSolver, DomainId, DomainId),
) -> Result<(), FlatZincError> {
    check_parameters!(exprs, 2, predicate_name);

    let a = context.resolve_integer_variable(&exprs[0])?;
    let b = context.resolve_integer_variable(&exprs[1])?;

    post_constraint(context.solver, a, b);

    Ok(())
}

fn compile_reified_binary_int_predicate(
    context: &mut CompilationContext,
    exprs: &[flatzinc::Expr],
    _: &[flatzinc::Annotation],
    predicate_name: &str,
    post_constraint: impl FnOnce(&mut ConstraintSatisfactionSolver, DomainId, DomainId, Literal),
) -> Result<(), FlatZincError> {
    check_parameters!(exprs, 3, predicate_name);

    let a = context.resolve_integer_variable(&exprs[0])?;
    let b = context.resolve_integer_variable(&exprs[1])?;
    let reif = context.resolve_bool_variable(&exprs[2])?;

    post_constraint(context.solver, a, b, reif);

    Ok(())
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
    post_constraint: impl FnOnce(&mut ConstraintSatisfactionSolver, Box<[AffineView<DomainId>]>, i32),
) -> Result<(), FlatZincError> {
    check_parameters!(exprs, 3, predicate_name);

    let weights = context.resolve_array_integer_constants(&exprs[0])?;
    let vars = context.resolve_integer_variable_array(&exprs[1])?;
    let rhs = context.resolve_integer_constant_from_expr(&exprs[2])?;

    let terms = weighted_vars(weights, vars);

    post_constraint(context.solver, terms, rhs);

    Ok(())
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
    ),
) -> Result<(), FlatZincError> {
    check_parameters!(exprs, 4, predicate_name);

    let weights = context.resolve_array_integer_constants(&exprs[0])?;
    let vars = context.resolve_integer_variable_array(&exprs[1])?;
    let rhs = context.resolve_integer_constant_from_expr(&exprs[2])?;
    let reif = context.resolve_bool_variable(&exprs[3])?;

    let terms = weighted_vars(weights, vars);

    post_constraint(context.solver, terms, rhs, reif);

    Ok(())
}

fn compile_all_different(
    context: &mut CompilationContext,
    exprs: &[flatzinc::Expr],
    _: &[flatzinc::Annotation],
) -> Result<(), FlatZincError> {
    check_parameters!(exprs, 1, "fzn_all_different");

    let variables = context.resolve_integer_variable_array(&exprs[0])?.to_vec();
    context.solver.all_different(variables);

    Ok(())
}
