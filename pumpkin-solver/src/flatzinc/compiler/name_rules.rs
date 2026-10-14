//! Naming the rules of fzn-croc-check that license the inferences from constraints whose
//! propagators carry another label. The constraints are collected before any is consumed, and
//! named once their variables exist.

use super::context::CompilationContext;
use crate::core::proof::ConstraintTag;
use crate::flatzinc::ProofFormat;
use crate::flatzinc::error::FlatZincError;

/// The constraints of the model, before compilation consumes any.
pub(crate) fn collect(
    context: &CompilationContext,
) -> Vec<(ConstraintTag, String, Vec<flatzinc::Expr>)> {
    context
        .constraints
        .iter()
        .map(|(constraint_tag, item)| (*constraint_tag, item.id.clone(), item.exprs.clone()))
        .collect()
}

/// Whether the expression is a constant or a variable the compilation still knows; a variable
/// that was removed cannot be resolved.
fn is_known(context: &CompilationContext, expression: &flatzinc::Expr) -> bool {
    match expression {
        flatzinc::Expr::VarParIdentifier(identifier) => context.equivalences.is_defined(identifier),
        _ => true,
    }
}

pub(crate) fn run(
    context: &mut CompilationContext,
    constraints: Vec<(ConstraintTag, String, Vec<flatzinc::Expr>)>,
) -> Result<(), FlatZincError> {
    if context.proof_format != ProofFormat::Croc || !context.solver.is_logging_proof() {
        return Ok(());
    }
    for (constraint_tag, id, exprs) in constraints {
        // fzn-croc-check reads a reified equality or disequality of two terms as a reified linear
        // comparison: an inference whose nogood contains the indicator is licensed by the
        // comparison, one that contains its negation by the negated comparison.
        // An equality of two constants has no term for the rule of a two-term equality, and the
        // bounds rule refutes it.
        let equal_rule = if exprs
            .iter()
            .take(2)
            .all(|expr| matches!(expr, flatzinc::Expr::Bool(_) | flatzinc::Expr::Int(_)))
        {
            "linear_bounds"
        } else {
            "linear_equal_binary"
        };
        let sides = match id.as_str() {
            "int_eq_reif" | "bool_eq_reif" => Some((equal_rule, "linear_not_equal")),
            "int_ne_reif" | "pumpkin_bool_xor_reif" => Some(("linear_not_equal", equal_rule)),
            _ => None,
        };
        if let (Some((when_true, when_false)), Some(indicator)) = (
            sides,
            exprs
                .get(2)
                .filter(|expr| is_known(context, expr))
                .and_then(|expr| context.resolve_bool_variable(expr).ok()),
        ) {
            context.solver.name_rules_by_indicator_side(
                constraint_tag,
                indicator.get_true_predicate(),
                when_true,
                when_false,
            );
            // The clauses of the decomposition leave out that the two booleans are 0/1, which
            // the disequality under the indicator needs to see both terms fixed.
            if id == "pumpkin_bool_xor_reif" {
                context.solver.state_fixing_bounds_when_true(constraint_tag);
            }
        }
        // An equality of two constants has no term over a variable, which the rule for a linear
        // equality of two terms needs; the bounds rule refutes it.
        if matches!(id.as_str(), "int_eq" | "bool_eq" | "bool2int")
            && exprs
                .iter()
                .take(2)
                .all(|expr| matches!(expr, flatzinc::Expr::Bool(_) | flatzinc::Expr::Int(_)))
        {
            context
                .solver
                .name_rule_of_constraint(constraint_tag, "linear_bounds");
        }
        // A product with a constant factor is the linear equality product - factor * other = 0.
        if id == "int_times"
            && exprs.iter().take(2).any(|expr| {
                is_known(context, expr)
                    && context.resolve_integer_variable(expr).is_ok_and(|view| {
                        context.solver.lower_bound(&view) == context.solver.upper_bound(&view)
                    })
            })
        {
            context
                .solver
                .name_rule_of_constraint(constraint_tag, "linear_bounds");
        }
    }
    Ok(())
}
