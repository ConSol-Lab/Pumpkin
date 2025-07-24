//! Add objective function to solver

use super::context::CompilationContext;
use crate::flatzinc::ast::Instance;
use crate::flatzinc::instance::FlatzincObjective;
use crate::flatzinc::FlatZincError;

pub(crate) fn run(
    typed_ast: &Instance,
    context: &mut CompilationContext,
) -> Result<Option<FlatzincObjective>, FlatZincError> {
    match &typed_ast.solve.method.node {
        fzn_rs::Method::Satisfy => Ok(None),
        fzn_rs::Method::Optimize {
            direction,
            objective,
        } => {
            let variable = context.resolve_integer_variable(objective)?;

            match direction {
                fzn_rs::ast::OptimizationDirection::Minimize => {
                    Ok(Some(FlatzincObjective::Minimize(variable)))
                }
                fzn_rs::ast::OptimizationDirection::Maximize => {
                    Ok(Some(FlatzincObjective::Maximize(variable)))
                }
            }
        }
    }
}
