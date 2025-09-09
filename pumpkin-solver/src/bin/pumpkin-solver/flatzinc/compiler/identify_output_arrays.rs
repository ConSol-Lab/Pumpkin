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
            ArrayAnnotations::OutputArray(array_expr) => {
                let shape = instance
                    .resolve_array(array_expr)
                    .map_err(|err| FlatZincError::UndefinedArray(err.0))
                    .and_then(|iter| iter.collect::<Result<Vec<_>, _>>().map_err(Into::into))
                    .map(parse_array_shape);

                Some(shape)
            }
        }) else {
            continue;
        };

        let shape = shape?;

        let output = match array.domain.node {
            ast::Domain::UnboundedInt | ast::Domain::Int(_) => {
                let variables = array
                    .contents
                    .iter()
                    .map(|node| {
                        let variable = <VariableExpr<i32> as FromLiteral>::from_literal(node)?;

                        let solver_variable = context.resolve_integer_variable(&variable)?;
                        Ok(solver_variable)
                    })
                    .collect::<Result<Vec<_>, FlatZincError>>()?;

                Output::array_of_int(Rc::clone(name), shape.clone(), variables)
            }

            ast::Domain::Bool => {
                let variables = array
                    .contents
                    .iter()
                    .map(|node| {
                        let variable = <VariableExpr<bool> as FromLiteral>::from_literal(node)?;

                        let solver_variable = context.resolve_bool_variable(&variable)?;
                        Ok(solver_variable)
                    })
                    .collect::<Result<Vec<_>, FlatZincError>>()?;

                Output::array_of_bool(Rc::clone(name), shape.clone(), variables)
            }
        };

        context.outputs.push(output);
    }

    Ok(())
}

/// Parse an array of ranges, which is the argument to the `output_array` annotation, to a slice of
/// pairs which is expect by our output system.
fn parse_array_shape(ranges: Vec<RangeList<i32>>) -> Box<[(i32, i32)]> {
    ranges
        .iter()
        .map(|ranges| (*ranges.lower_bound(), *ranges.upper_bound()))
        .collect()
}
