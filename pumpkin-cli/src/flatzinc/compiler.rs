use log::warn;
use pumpkin_lib::{
    basic_types::{variables::IntVar, DomainId},
    engine::ConstraintSatisfactionSolver,
};

use super::{
    constraints,
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
    for (id, bounds) in instance.iter_variables() {
        let domain_id = solver.create_new_integer_variable(*bounds.start(), *bounds.end());
        variable_map.register_integer_variable(id.clone(), domain_id);
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
        "int_lin_ne" => compile_int_lin_ne(variable_map, instance, exprs, annos, solver),
        unknown => todo!("unsupported constraint {unknown}"),
    }
}

fn compile_int_lin_ne(
    variable_map: &VariableMap,
    instance: &FlatZincInstance,
    exprs: &[flatzinc::Expr],
    annos: &[flatzinc::Annotation],
    solver: &mut ConstraintSatisfactionSolver,
) -> Result<(), FlatZincError> {
    if !annos.is_empty() {
        warn!("ignoring annotations on int_lin_ne");
    }

    if exprs.len() != 3 {
        return Err(FlatZincError::IncorrectNumberOfArguments {
            constraint_id: "int_lin_ne".into(),
            expected: 3,
            actual: exprs.len(),
        });
    }

    let weights = resolve_array_integer_constants(instance, &exprs[0])?;
    let vars = resolve_variable_array(variable_map, instance, &exprs[1])?;
    let c = resolve_integer_constant(instance, &exprs[2])?;

    let x = vars
        .iter()
        .zip(weights.iter())
        .map(|(x_i, &w_i)| x_i.scaled(w_i))
        .collect();

    constraints::int_lin_ne(solver, x, c);

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
                    .map(|elem_id| resolve_integer_variable(variable_map, elem_id))
                    .collect::<Result<_, _>>()
            }),

        // The AST is not correct here. Since an in-place array will likely only contain
        // identifiers, and this variant is the first one that is attempted, an array of bool
        // expressions is returned even if the identifiers refer to integers.
        flatzinc::Expr::ArrayOfBool(array) => array
            .iter()
            .map(|elem| {
                if let flatzinc::BoolExpr::VarParIdentifier(id) = elem {
                    resolve_integer_variable(variable_map, id)
                } else {
                    Err(FlatZincError::UnexpectedExpr)
                }
            })
            .collect(),
        _ => Err(FlatZincError::UnexpectedExpr),
    }
}

fn resolve_integer_variable(
    variable_map: &VariableMap,
    identifier: &str,
) -> Result<DomainId, FlatZincError> {
    variable_map
        .resolve(identifier)
        .map(|var| match var {
            Variable::Integer(domain) => domain,
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
