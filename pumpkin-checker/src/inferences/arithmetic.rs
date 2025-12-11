use std::collections::BTreeSet;
use std::rc::Rc;

use drcp_format::IntComparison;
use fzn_rs::VariableExpr;

use super::Fact;
use crate::inferences::InvalidInference;
use crate::model::AllDifferent;
use crate::model::Atomic;
use crate::model::Constraint;
use crate::state::I32Ext;
use crate::state::VariableState;

/// Verify a `binary_equals` inference.
///
/// The checker accepts inferences for binary equality constraints. The difference with the general
/// `linear_bounds` inference is that in the binary case, we can certify holes in the domain as
/// well.
pub(crate) fn verify_binary_equals(
    fact: &Fact,
    constraint: &Constraint,
) -> Result<(), InvalidInference> {
    // To check this inference we expect the intersection of both domains to be empty.

    let Constraint::LinearEq(linear) = constraint else {
        return Err(InvalidInference::ConstraintLabelMismatch);
    };

    // For now, this inference only works for constraints over two variables.
    if linear.terms.len() != 2 {
        return Err(InvalidInference::Unsound);
    }

    let (weight_a, variable_a) = &linear.terms[0];
    let (weight_b, variable_b) = &linear.terms[1];

    // TODO: Generalize this rule to work with non-unit weights.
    // At the moment we expect one term to have weight `-1` and the other term to have weight
    // `1`.
    if weight_a + weight_b != 0 || weight_a.abs() != 1 || weight_b.abs() != 1 {
        return Err(InvalidInference::Unsound);
    }

    let mut variable_state = VariableState::prepare_for_conflict_check(fact)
        .ok_or(InvalidInference::InconsistentPremises)?;

    // We apply the domain of variable 2 to variable 1. If the state remains consistent, then
    // the step is unsound!
    let state_is_consistent = match variable_a {
        VariableExpr::Identifier(var1) => {
            let mut consistent = true;

            if let I32Ext::I32(value) = variable_state.upper_bound(variable_b) {
                consistent &= variable_state.apply(&Atomic {
                    name: Rc::clone(var1),
                    comparison: IntComparison::LessEqual,
                    value: linear.bound + value,
                });
            }

            if let I32Ext::I32(value) = variable_state.lower_bound(variable_b) {
                consistent &= variable_state.apply(&Atomic {
                    name: Rc::clone(var1),
                    comparison: IntComparison::GreaterEqual,
                    value: linear.bound + value,
                });
            }

            for value in variable_state.holes(variable_b).collect::<Vec<_>>() {
                consistent &= variable_state.apply(&Atomic {
                    name: Rc::clone(var1),
                    comparison: IntComparison::NotEqual,
                    value: linear.bound + value,
                });
            }

            consistent
        }

        VariableExpr::Constant(value) => match variable_b {
            VariableExpr::Identifier(var2) => variable_state.apply(&Atomic {
                name: Rc::clone(var2),
                comparison: IntComparison::NotEqual,
                value: linear.bound + *value,
            }),
            VariableExpr::Constant(_) => panic!("Binary equals over two constants is unexpected."),
        },
    };

    if state_is_consistent {
        // The intersection of the domains should yield an inconsistent state for the
        // inference to be sound.
        return Err(InvalidInference::Unsound);
    }

    Ok(())
}

/// Verify a `binary_not_equals` inference.
///
/// Tests that the premise of the inference and the negation of the consequent force the linear sum
/// to equal the right-hand side of the not equals constraint.
pub(crate) fn verify_binary_not_equals(
    fact: &Fact,
    constraint: &Constraint,
) -> Result<(), InvalidInference> {
    let Constraint::AllDifferent(AllDifferent { variables }) = constraint else {
        return Err(InvalidInference::ConstraintLabelMismatch);
    };

    let variable_state = VariableState::prepare_for_conflict_check(fact)
        .ok_or(InvalidInference::InconsistentPremises)?;

    let mut values = BTreeSet::new();
    for variable in variables {
        let Some(value) = variable_state.fixed_value(variable) else {
            continue;
        };

        if !values.insert(value) {
            return Ok(());
        }
    }

    Err(InvalidInference::Unsound)
}
