use log::warn;
use pumpkin_lib::{
    basic_types::{
        variables::{AffineView, IntVar},
        DomainId, Literal,
    },
    constraints::ConstraintsExt,
    engine::ConstraintSatisfactionSolver,
};

use super::{
    instance::{FlatZincInstance, Variable, VariableMap},
    FlatZincError,
};

pub fn compile(
    instance: &FlatZincInstance,
    solver: &mut ConstraintSatisfactionSolver,
) -> Result<VariableMap, FlatZincError> {
    let mut variable_map = VariableMap::default();

    // Go through variables and add an explicit DomainId for every variable. A possible
    // optimization is to replace functionally defined variables by views.
    for (id, bounds) in instance.iter_integer_variables() {
        let domain_id = solver.create_new_integer_variable(*bounds.start(), *bounds.end());
        variable_map.register_integer_variable(id.clone(), domain_id);
    }

    for id in instance.iter_bool_variables() {
        let literal = Literal::new(solver.create_new_propositional_variable(), true);
        variable_map.register_bool_variable(id.clone(), literal);
    }

    for constraint in instance.iter_constraints() {
        compile_constraint(&variable_map, instance, constraint, solver)?;
    }

    Ok(variable_map)
}

fn compile_constraint(
    variable_map: &VariableMap,
    instance: &FlatZincInstance,
    constraint: &flatzinc::ConstraintItem,
    solver: &mut ConstraintSatisfactionSolver,
) -> Result<(), FlatZincError> {
    let flatzinc::ConstraintItem { id, exprs, annos } = constraint;

    match id.as_str() {
        "int_lin_ne" => compile_int_lin_predicate(
            variable_map,
            instance,
            exprs,
            annos,
            "int_lin_ne",
            |terms, rhs| solver.int_lin_ne(terms, rhs),
        ),
        "int_lin_ne_reif" => todo!("the LinearNe propagator does not yet support reification"),
        "int_lin_le" => compile_int_lin_predicate(
            variable_map,
            instance,
            exprs,
            annos,
            "int_lin_le",
            |terms, rhs| solver.int_lin_le(terms, rhs),
        ),
        "int_lin_le_reif" => compile_reified_int_lin_predicate(
            variable_map,
            instance,
            exprs,
            annos,
            "int_lin_le_reif",
            |terms, rhs, reif| solver.int_lin_le_reif(terms, rhs, reif),
        ),
        "int_lin_eq" => compile_int_lin_predicate(
            variable_map,
            instance,
            exprs,
            annos,
            "int_lin_eq",
            |terms, rhs| solver.int_lin_eq(terms, rhs),
        ),
        "int_lin_eq_reif" => compile_reified_int_lin_predicate(
            variable_map,
            instance,
            exprs,
            annos,
            "int_lin_eq_reif",
            |terms, rhs, reif| solver.int_lin_eq_reif(terms, rhs, reif),
        ),
        "int_ne" => compile_binary_int_predicate(variable_map, exprs, annos, "int_ne", |a, b| {
            solver.int_ne(a, b)
        }),
        "int_ne_reif" => todo!("the LinearNe propagator does not yet support reification"),
        "int_le" => compile_binary_int_predicate(variable_map, exprs, annos, "int_le", |a, b| {
            solver.int_le(a, b)
        }),
        "int_le_reif" => compile_reified_binary_int_predicate(
            variable_map,
            exprs,
            annos,
            "int_le_reif",
            |a, b, reif| solver.int_le_reif(a, b, reif),
        ),
        "int_lt" => compile_binary_int_predicate(variable_map, exprs, annos, "int_lt", |a, b| {
            solver.int_lt(a, b)
        }),
        "int_lt_reif" => compile_reified_binary_int_predicate(
            variable_map,
            exprs,
            annos,
            "int_lt_reif",
            |a, b, reif| solver.int_lt_reif(a, b, reif),
        ),
        "int_eq" => compile_binary_int_predicate(variable_map, exprs, annos, "int_eq", |a, b| {
            solver.int_eq(a, b)
        }),
        "int_eq_reif" => compile_reified_binary_int_predicate(
            variable_map,
            exprs,
            annos,
            "int_eq_reif",
            |a, b, reif| solver.int_eq_reif(a, b, reif),
        ),
        "int_plus" => compile_int_plus(variable_map, exprs, annos, solver),
        "int_times" => compile_int_times(variable_map, exprs, annos, solver),
        "fzn_all_different_int" => {
            compile_all_different(variable_map, instance, exprs, annos, solver)
        }
        unknown => todo!("unsupported constraint {unknown}"),
    }
}

fn compile_int_plus(
    variable_map: &VariableMap,
    exprs: &[flatzinc::Expr],
    annos: &[flatzinc::Annotation],
    solver: &mut ConstraintSatisfactionSolver,
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

    let a = resolve_integer_variable(variable_map, &exprs[0])?;
    let b = resolve_integer_variable(variable_map, &exprs[1])?;
    let c = resolve_integer_variable(variable_map, &exprs[2])?;

    solver.int_plus(a, b, c);

    Ok(())
}

fn compile_int_times(
    variable_map: &VariableMap,
    exprs: &[flatzinc::Expr],
    annos: &[flatzinc::Annotation],
    solver: &mut ConstraintSatisfactionSolver,
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

    let a = resolve_integer_variable(variable_map, &exprs[0])?;
    let b = resolve_integer_variable(variable_map, &exprs[1])?;
    let c = resolve_integer_variable(variable_map, &exprs[2])?;

    solver.int_times(a, b, c);

    Ok(())
}

fn compile_binary_int_predicate(
    variable_map: &VariableMap,
    exprs: &[flatzinc::Expr],
    annos: &[flatzinc::Annotation],
    predicate_name: &str,
    post_constraint: impl FnOnce(DomainId, DomainId),
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

    let a = resolve_integer_variable(variable_map, &exprs[0])?;
    let b = resolve_integer_variable(variable_map, &exprs[1])?;

    post_constraint(a, b);

    Ok(())
}

fn compile_reified_binary_int_predicate(
    variable_map: &VariableMap,
    exprs: &[flatzinc::Expr],
    annos: &[flatzinc::Annotation],
    predicate_name: &str,
    post_constraint: impl FnOnce(DomainId, DomainId, Literal),
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

    let a = resolve_integer_variable(variable_map, &exprs[0])?;
    let b = resolve_integer_variable(variable_map, &exprs[1])?;
    let reif = resolve_bool_variable(variable_map, &exprs[2])?;

    post_constraint(a, b, reif);

    Ok(())
}

fn compile_int_lin_predicate(
    variable_map: &VariableMap,
    instance: &FlatZincInstance,
    exprs: &[flatzinc::Expr],
    annos: &[flatzinc::Annotation],
    predicate_name: &str,
    post_constraint: impl FnOnce(Box<[AffineView<DomainId>]>, i32),
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

    let weights = resolve_array_integer_constants(instance, &exprs[0])?;
    let vars = resolve_variable_array(variable_map, instance, &exprs[1])?;
    let rhs = resolve_integer_constant(instance, &exprs[2])?;

    let terms = vars
        .iter()
        .zip(weights.iter())
        .map(|(x_i, &w_i)| x_i.scaled(w_i))
        .collect::<Box<[_]>>();

    post_constraint(terms, rhs);

    Ok(())
}

fn compile_reified_int_lin_predicate(
    variable_map: &VariableMap,
    instance: &FlatZincInstance,
    exprs: &[flatzinc::Expr],
    annos: &[flatzinc::Annotation],
    predicate_name: &str,
    post_constraint: impl FnOnce(Box<[AffineView<DomainId>]>, i32, Literal),
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

    let weights = resolve_array_integer_constants(instance, &exprs[0])?;
    let vars = resolve_variable_array(variable_map, instance, &exprs[1])?;
    let rhs = resolve_integer_constant(instance, &exprs[2])?;
    let reif = resolve_bool_variable(variable_map, &exprs[3])?;

    let terms = vars
        .iter()
        .zip(weights.iter())
        .map(|(x_i, &w_i)| x_i.scaled(w_i))
        .collect::<Box<[_]>>();

    post_constraint(terms, rhs, reif);

    Ok(())
}

fn compile_all_different(
    variable_map: &VariableMap,
    instance: &FlatZincInstance,
    exprs: &[flatzinc::Expr],
    annos: &[flatzinc::Annotation],
    solver: &mut ConstraintSatisfactionSolver,
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

    let variables = resolve_variable_array(variable_map, instance, &exprs[0])?;
    solver.all_different(variables);

    Ok(())
}

fn resolve_integer_constant(
    instance: &FlatZincInstance,
    expr: &flatzinc::Expr,
) -> Result<i32, FlatZincError> {
    match expr {
        flatzinc::Expr::VarParIdentifier(id) => {
            instance
                .resolve_integer_constant(id)
                .ok_or_else(|| FlatZincError::InvalidIdentifier {
                    identifier: id.as_str().into(),
                    expected_type: "constant integer".into(),
                })
        }
        flatzinc::Expr::Int(value) => i32::try_from(*value).map_err(Into::into),
        _ => Err(FlatZincError::UnexpectedExpr),
    }
}

fn resolve_variable_array(
    variable_map: &VariableMap,
    instance: &FlatZincInstance,
    expr: &flatzinc::Expr,
) -> Result<Box<[DomainId]>, FlatZincError> {
    match expr {
        flatzinc::Expr::VarParIdentifier(id) => instance
            .resolve_variable_array(id)
            .ok_or_else(|| FlatZincError::InvalidIdentifier {
                identifier: id.as_str().into(),
                expected_type: "integer variable array".into(),
            })
            .and_then(|array| {
                array
                    .iter()
                    .map(|elem_id| resolve_integer_variable_from_identifier(variable_map, elem_id))
                    .collect::<Result<_, _>>()
            }),

        // The AST is not correct here. Since an in-place array will likely only contain
        // identifiers, and this variant is the first one that is attempted, an array of bool
        // expressions is returned even if the identifiers refer to integers.
        flatzinc::Expr::ArrayOfBool(array) => array
            .iter()
            .map(|elem| {
                if let flatzinc::BoolExpr::VarParIdentifier(id) = elem {
                    resolve_integer_variable_from_identifier(variable_map, id)
                } else {
                    Err(FlatZincError::UnexpectedExpr)
                }
            })
            .collect(),
        _ => Err(FlatZincError::UnexpectedExpr),
    }
}

fn resolve_bool_variable(
    variable_map: &VariableMap,
    expr: &flatzinc::Expr,
) -> Result<Literal, FlatZincError> {
    match expr {
        flatzinc::Expr::VarParIdentifier(id) => {
            resolve_bool_variable_from_identifier(variable_map, id)
        }

        _ => Err(FlatZincError::UnexpectedExpr),
    }
}

fn resolve_integer_variable(
    variable_map: &VariableMap,
    expr: &flatzinc::Expr,
) -> Result<DomainId, FlatZincError> {
    match expr {
        flatzinc::Expr::VarParIdentifier(id) => {
            resolve_integer_variable_from_identifier(variable_map, id)
        }

        _ => Err(FlatZincError::UnexpectedExpr),
    }
}

fn resolve_bool_variable_from_identifier(
    variable_map: &VariableMap,
    identifier: &str,
) -> Result<Literal, FlatZincError> {
    variable_map
        .resolve(identifier)
        .and_then(|var| match var {
            Variable::Bool(literal) => Some(literal),
            _ => None,
        })
        .ok_or_else(|| FlatZincError::InvalidIdentifier {
            identifier: identifier.into(),
            expected_type: "bool variable".into(),
        })
}

fn resolve_integer_variable_from_identifier(
    variable_map: &VariableMap,
    identifier: &str,
) -> Result<DomainId, FlatZincError> {
    variable_map
        .resolve(identifier)
        .and_then(|var| match var {
            Variable::Integer(domain) => Some(domain),
            _ => None,
        })
        .ok_or_else(|| FlatZincError::InvalidIdentifier {
            identifier: identifier.into(),
            expected_type: "integer variable".into(),
        })
}

fn resolve_array_integer_constants(
    instance: &FlatZincInstance,
    expr: &flatzinc::Expr,
) -> Result<Box<[i32]>, FlatZincError> {
    match expr {
        flatzinc::Expr::VarParIdentifier(id) => instance
            .resolve_array_integer_constants(id)
            .ok_or_else(|| FlatZincError::InvalidIdentifier {
                identifier: id.as_str().into(),
                expected_type: "constant integer array".into(),
            }),
        flatzinc::Expr::ArrayOfInt(exprs) => exprs
            .iter()
            .map(|e| resolve_int_expr(instance, e))
            .collect::<Result<Box<[i32]>, _>>(),
        _ => Err(FlatZincError::UnexpectedExpr),
    }
}

fn resolve_int_expr(
    instance: &FlatZincInstance,
    expr: &flatzinc::IntExpr,
) -> Result<i32, FlatZincError> {
    match expr {
        flatzinc::IntExpr::Int(value) => i32::try_from(*value).map_err(Into::into),
        flatzinc::IntExpr::VarParIdentifier(id) => instance
            .resolve_integer_constant(id)
            .ok_or_else(|| FlatZincError::InvalidIdentifier {
                identifier: id.as_str().into(),
                expected_type: "constant integer".into(),
            }),
    }
}
