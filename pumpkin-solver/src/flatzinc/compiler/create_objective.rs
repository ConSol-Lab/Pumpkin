//! Add objective function to solver

use flatzinc::BoolExpr;
use flatzinc::Goal;

use super::context::CompilationContext;
use crate::flatzinc::FlatZincError;
use crate::flatzinc::ast::FlatZincAst;
use crate::flatzinc::instance::FlatzincObjective;

pub(crate) fn run(
    ast: &FlatZincAst,
    context: &mut CompilationContext,
) -> Result<Option<FlatzincObjective>, FlatZincError> {
    match &ast.solve_item.goal {
        Goal::Satisfy => Ok(None),
        Goal::OptimizeBool(optimization_type, bool_expr) => {
            // The objective function will be parsed as a bool because that is the first identifier
            // it will find For now we assume that the objective function is a single
            // integer

            let domain = match bool_expr {
                BoolExpr::Bool(_) => unreachable!(
                    "We do not expect a constant to be present in the objective function!"
                ),
                BoolExpr::VarParIdentifier(x) => {
                    if context.is_identifier_parameter(x) {
                        context.resolve_integer_constant_from_identifier(x)?
                    } else {
                        context.resolve_integer_variable_from_identifier(x)?
                    }
                }
            };

            Ok(Some(match optimization_type {
                flatzinc::OptimizationType::Minimize => FlatzincObjective::Minimize(domain),
                flatzinc::OptimizationType::Maximize => FlatzincObjective::Maximize(domain),
            }))
        }
        _ => todo!(
            "For now we assume that the optimisation function is a single integer to optimise"
        ),
    }
}
