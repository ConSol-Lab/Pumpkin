//! Collects every FlatZinc variable of the model with what gives its value in a solution, so that
//! a solution can be written in full and checked against the whole model. Reads the compiled
//! model only; it changes nothing in the solver.

use std::rc::Rc;

use super::context::CompilationContext;
use crate::core::variables::Literal;
use crate::core::variables::TransformableVariable;
use crate::flatzinc::ast::FlatZincAst;
use crate::flatzinc::ast::SingleVarDecl;
use crate::flatzinc::instance::FullSolution;
use crate::flatzinc::instance::SolutionValue;

pub(crate) fn run(ast: &FlatZincAst, context: &mut CompilationContext) -> FullSolution {
    let mut full_solution = FullSolution::default();
    for decl in &ast.single_variables {
        let (id, is_bool, lower_bound) = match decl {
            SingleVarDecl::Bool { id, .. } => (id, true, "false".to_owned()),
            SingleVarDecl::IntInRange { id, lb, .. } => (id, false, lb.to_string()),
            SingleVarDecl::IntInSet { id, set, .. } => (
                id,
                false,
                set.iter().min().copied().unwrap_or(0).to_string(),
            ),
        };
        let value = value_of(context, id, is_bool).unwrap_or(SolutionValue::Fixed(lower_bound));
        full_solution.push(context.identifiers.get_interned(id), value);
    }
    for unused in &context.unused_variables {
        // An unused variable assigned another takes that one's value.
        let value = unused
            .alias
            .as_ref()
            .and_then(|alias| value_of(context, alias, unused.is_bool))
            .unwrap_or_else(|| SolutionValue::Fixed(unused.value.clone()));
        full_solution.push(Rc::clone(&unused.name), value);
    }
    full_solution
}

/// What gives the value of the variable in a solution, as the compiler resolves it.
fn value_of(context: &CompilationContext, id: &str, is_bool: bool) -> Option<SolutionValue> {
    if let Some(&literal) = context.literal_booleans.get(id) {
        return Some(SolutionValue::Bool(literal));
    }
    if !context.equivalences.classes.contains_key(id) {
        return None;
    }
    let (representative, view) = context.equivalences.view(id);
    if let Some(&domain_id) = context.variable_map.get(&representative) {
        return Some(if is_bool {
            match (view.scale, view.offset) {
                (1, 0) => SolutionValue::Bool(Literal::new(domain_id)),
                (-1, 1) => SolutionValue::Bool(!Literal::new(domain_id)),
                _ => return None,
            }
        } else {
            SolutionValue::Int(domain_id.scaled(view.scale).offset(view.offset))
        });
    }
    let value = context
        .integer_parameters
        .get(&representative)
        .map(|&value| i64::from(value))
        .or_else(|| {
            context
                .boolean_parameters
                .get(&representative)
                .map(|&value| i64::from(value))
        })?;
    let value = i64::from(view.scale) * value + i64::from(view.offset);
    Some(SolutionValue::Fixed(if is_bool {
        (value != 0).to_string()
    } else {
        value.to_string()
    }))
}
