use std::rc::Rc;

use fzn_rs::ast::RangeList;
use fzn_rs::ast::{self};
use fzn_rs::FromLiteral;
use fzn_rs::VariableExpr;

use super::CompilationContext;
use crate::flatzinc::ast::ArrayAnnotations;
use crate::flatzinc::ast::Instance;
use crate::flatzinc::error::FlatZincError;
use crate::flatzinc::instance::Output;

pub(crate) fn run(
    instance: &Instance,
    context: &mut CompilationContext,
) -> Result<(), FlatZincError> {
    for (name, array) in &instance.arrays {
        #[allow(
            clippy::unnecessary_find_map,
            reason = "it is only unnecessary because ArrayAnnotations has one variant"
        )]
        let Some(shape) = array.annotations.iter().find_map(|ann| match &ann.node {
            ArrayAnnotations::OutputArray(shape) => Some(parse_array_shape(shape)),
        }) else {
            continue;
        };

        // This is a bit hacky. We do not know easily whether the array is an array of
        // integers or booleans. So we try to resolve both, and then see which one works.
        let bool_array = resolve_array(array, |variable| context.resolve_bool_variable(variable));
        let int_array = resolve_array(array, |variable| context.resolve_integer_variable(variable));

        let output = match (bool_array, int_array) {
            (Ok(bools), Err(_)) => Output::array_of_bool(Rc::clone(name), shape.clone(), bools),
            (Err(_), Ok(ints)) => Output::array_of_int(Rc::clone(name), shape.clone(), ints),

            (Ok(_), Ok(_)) => unreachable!("Array of identifiers that are both integers and booleans"),
            (Err(e1), Err(e2)) => unreachable!("Array is neither of boolean or integer variables.\n\tBool error: {e1}\n\tInt error: {e2}"),
        };

        context.outputs.push(output);
    }

    Ok(())
}

fn resolve_array<Output, T, Ann>(
    array: &ast::Array<Ann>,
    mut resolve_single_variable: impl FnMut(&VariableExpr<T>) -> Result<Output, FlatZincError>,
) -> Result<Vec<Output>, FlatZincError>
where
    VariableExpr<T>: FromLiteral,
{
    array
        .contents
        .iter()
        .map(|node| {
            let variable = <VariableExpr<T> as FromLiteral>::from_literal(node)?;

            let solver_variable = resolve_single_variable(&variable)?;
            Ok(solver_variable)
        })
        .collect()
}

/// Parse an array of ranges, which is the argument to the `output_array` annotation, to a slice of
/// pairs which is expect by our output system.
fn parse_array_shape(ranges: &[RangeList<i32>]) -> Box<[(i32, i32)]> {
    ranges
        .iter()
        .map(|ranges| (*ranges.lower_bound(), *ranges.upper_bound()))
        .collect()
}
