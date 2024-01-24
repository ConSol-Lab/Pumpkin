//! Compile constraints into CP propagators

use log::warn;
use pumpkin_lib::basic_types::variables::AffineView;
use pumpkin_lib::basic_types::variables::IntVar;
use pumpkin_lib::basic_types::DomainId;
use pumpkin_lib::basic_types::Literal;
use pumpkin_lib::constraints::ConstraintsExt;
use pumpkin_lib::engine::ConstraintSatisfactionSolver;

use super::context::CompilationContext;
use crate::flatzinc::ast::FlatZincAst;
use crate::flatzinc::FlatZincError;

pub fn run(ast: &FlatZincAst, context: &mut CompilationContext) -> Result<(), FlatZincError> {
    for constraint_item in &ast.constraint_decls {
        let flatzinc::ConstraintItem { id, exprs, annos } = constraint_item;

        match id.as_str() {
            "int_lin_ne" => {
                compile_int_lin_predicate(
                    context,
                    exprs,
                    annos,
                    "int_lin_ne",
                    |solver, terms, rhs| solver.int_lin_ne(terms, rhs),
                )?;
            }
            "int_lin_ne_reif" => todo!("the LinearNe propagator does not yet support reification"),
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
                |solver, terms, rhs, reif| solver.int_lin_le_reif(terms, rhs, reif),
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
                |solver, terms, rhs, reif| solver.int_lin_eq_reif(terms, rhs, reif),
            )?,
            "int_ne" => {
                compile_binary_int_predicate(context, exprs, annos, "int_ne", |solver, a, b| {
                    solver.int_ne(a, b)
                })?
            }
            "int_ne_reif" => todo!("the LinearNe propagator does not yet support reification"),
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
                |solver, a, b, reif| solver.int_le_reif(a, b, reif),
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
                |solver, a, b, reif| solver.int_lt_reif(a, b, reif),
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
                |solver, a, b, reif| solver.int_eq_reif(a, b, reif),
            )?,
            "int_plus" => compile_int_plus(context, exprs, annos)?,
            "int_times" => compile_int_times(context, exprs, annos)?,
            "fzn_all_different_int" => compile_all_different(context, exprs, annos)?,
            unknown => todo!("unsupported constraint {unknown}"),
        }
    }

    Ok(())
}

fn compile_int_plus(
    context: &mut CompilationContext,
    exprs: &[flatzinc::Expr],
    annos: &[flatzinc::Annotation],
) -> Result<(), FlatZincError> {
    if !annos.is_empty() {
        warn!("ignoring annotations on int_plus");
    }

    if exprs.len() != 3 {
        return Err(FlatZincError::IncorrectNumberOfArguments {
            constraint_id: "int_plus".into(),
            expected: 3,
            actual: exprs.len(),
        });
    }

    let a = context.resolve_integer_variable(&exprs[0])?;
    let b = context.resolve_integer_variable(&exprs[1])?;
    let c = context.resolve_integer_variable(&exprs[2])?;

    context.solver.int_plus(a, b, c);

    Ok(())
}

fn compile_int_times(
    context: &mut CompilationContext,
    exprs: &[flatzinc::Expr],
    annos: &[flatzinc::Annotation],
) -> Result<(), FlatZincError> {
    if !annos.is_empty() {
        warn!("ignoring annotations on int_times");
    }

    if exprs.len() != 3 {
        return Err(FlatZincError::IncorrectNumberOfArguments {
            constraint_id: "int_times".into(),
            expected: 3,
            actual: exprs.len(),
        });
    }

    let a = context.resolve_integer_variable(&exprs[0])?;
    let b = context.resolve_integer_variable(&exprs[1])?;
    let c = context.resolve_integer_variable(&exprs[2])?;

    context.solver.int_times(a, b, c);

    Ok(())
}

fn compile_binary_int_predicate(
    context: &mut CompilationContext,
    exprs: &[flatzinc::Expr],
    annos: &[flatzinc::Annotation],
    predicate_name: &str,
    post_constraint: impl FnOnce(&mut ConstraintSatisfactionSolver, DomainId, DomainId),
) -> Result<(), FlatZincError> {
    if !annos.is_empty() {
        warn!("ignoring annotations on {predicate_name}");
    }

    if exprs.len() != 2 {
        return Err(FlatZincError::IncorrectNumberOfArguments {
            constraint_id: predicate_name.into(),
            expected: 2,
            actual: exprs.len(),
        });
    }

    let a = context.resolve_integer_variable(&exprs[0])?;
    let b = context.resolve_integer_variable(&exprs[1])?;

    post_constraint(context.solver, a, b);

    Ok(())
}

fn compile_reified_binary_int_predicate(
    context: &mut CompilationContext,
    exprs: &[flatzinc::Expr],
    annos: &[flatzinc::Annotation],
    predicate_name: &str,
    post_constraint: impl FnOnce(&mut ConstraintSatisfactionSolver, DomainId, DomainId, Literal),
) -> Result<(), FlatZincError> {
    if !annos.is_empty() {
        warn!("ignoring annotations on {predicate_name}");
    }

    if exprs.len() != 3 {
        return Err(FlatZincError::IncorrectNumberOfArguments {
            constraint_id: predicate_name.into(),
            expected: 3,
            actual: exprs.len(),
        });
    }

    let a = context.resolve_integer_variable(&exprs[0])?;
    let b = context.resolve_integer_variable(&exprs[1])?;
    let reif = context.resolve_bool_variable(&exprs[2])?;

    post_constraint(context.solver, a, b, reif);

    Ok(())
}

fn compile_int_lin_predicate(
    context: &mut CompilationContext,
    exprs: &[flatzinc::Expr],
    annos: &[flatzinc::Annotation],
    predicate_name: &str,
    post_constraint: impl FnOnce(&mut ConstraintSatisfactionSolver, Box<[AffineView<DomainId>]>, i32),
) -> Result<(), FlatZincError> {
    if !annos.is_empty() {
        warn!("ignoring annotations on {predicate_name}");
    }

    if exprs.len() != 3 {
        return Err(FlatZincError::IncorrectNumberOfArguments {
            constraint_id: predicate_name.into(),
            expected: 3,
            actual: exprs.len(),
        });
    }

    let weights = context.resolve_array_integer_constants(&exprs[0])?;
    let vars = context.resolve_variable_array(&exprs[1])?;
    let rhs = context.resolve_integer_constant_from_expr(&exprs[2])?;

    let terms = vars
        .iter()
        .zip(weights.iter())
        .map(|(x_i, &w_i)| x_i.scaled(w_i))
        .collect::<Box<[_]>>();

    post_constraint(context.solver, terms, rhs);

    Ok(())
}

fn compile_reified_int_lin_predicate(
    context: &mut CompilationContext,
    exprs: &[flatzinc::Expr],
    annos: &[flatzinc::Annotation],
    predicate_name: &str,
    post_constraint: impl FnOnce(
        &mut ConstraintSatisfactionSolver,
        Box<[AffineView<DomainId>]>,
        i32,
        Literal,
    ),
) -> Result<(), FlatZincError> {
    if !annos.is_empty() {
        warn!("ignoring annotations on {predicate_name}");
    }

    if exprs.len() != 4 {
        return Err(FlatZincError::IncorrectNumberOfArguments {
            constraint_id: predicate_name.into(),
            expected: 4,
            actual: exprs.len(),
        });
    }

    let weights = context.resolve_array_integer_constants(&exprs[0])?;
    let vars = context.resolve_variable_array(&exprs[1])?;
    let rhs = context.resolve_integer_constant_from_expr(&exprs[2])?;
    let reif = context.resolve_bool_variable(&exprs[3])?;

    let terms = vars
        .iter()
        .zip(weights.iter())
        .map(|(x_i, &w_i)| x_i.scaled(w_i))
        .collect::<Box<[_]>>();

    post_constraint(context.solver, terms, rhs, reif);

    Ok(())
}

fn compile_all_different(
    context: &mut CompilationContext,
    exprs: &[flatzinc::Expr],
    annos: &[flatzinc::Annotation],
) -> Result<(), FlatZincError> {
    if !annos.is_empty() {
        warn!("ignoring annotations on fzn_all_different");
    }

    if exprs.len() != 1 {
        return Err(FlatZincError::IncorrectNumberOfArguments {
            constraint_id: "fzn_all_different".into(),
            expected: 1,
            actual: exprs.len(),
        });
    }

    let variables = context.resolve_variable_array(&exprs[0])?.to_vec();
    context.solver.all_different(variables);

    Ok(())
}
