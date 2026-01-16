use std::collections::BTreeSet;

use pumpkin_checking::CheckerVariable;
use pumpkin_checking::I32Ext;
use pumpkin_checking::VariableState;

use super::Fact;
use crate::inferences::InvalidInference;
use crate::model::AllDifferent;
use crate::model::Atomic;
use crate::model::Constraint;

/// Verify a `binary_equals` inference.
///
/// The checker accepts inferences for binary equality constraints. The difference with the general
/// `linear_bounds` inference is that in the binary case, we can certify holes in the domain as
/// well.
pub(crate) fn verify_binary_equals(
    _: &Fact,
    constraint: &Constraint,
    mut state: VariableState<Atomic>,
) -> Result<(), InvalidInference> {
    // To check this inference we expect the intersection of both domains to be empty.

    let Constraint::LinearEq(linear) = constraint else {
        return Err(InvalidInference::ConstraintLabelMismatch);
    };

    // For now, this inference only works for constraints over two variables.
    if linear.terms.len() != 2 {
        return Err(InvalidInference::Unsound);
    }

    let term_a = &linear.terms[0];
    let term_b = &linear.terms[1];

    let weight_a = term_a.weight.get();
    let weight_b = term_b.weight.get();

    // TODO: Generalize this rule to work with non-unit weights.
    // At the moment we expect one term to have weight `-1` and the other term to have weight
    // `1`.
    if weight_a + weight_b != 0 || weight_a.abs() != 1 || weight_b.abs() != 1 {
        return Err(InvalidInference::Unsound);
    }

    // We apply the domain of variable 2 to variable 1. If the state remains consistent, then
    // the step is unsound!
    let mut consistent = true;

    if let I32Ext::I32(value) = term_b.induced_upper_bound(&state) {
        let atomic = term_a.atomic_less_than(linear.bound + value);
        consistent &= state.apply(&atomic);
    }

    if let I32Ext::I32(value) = term_b.induced_lower_bound(&state) {
        let atomic = term_a.atomic_greater_than(linear.bound + value);
        consistent &= state.apply(&atomic);
    }

    for value in term_b.induced_holes(&state).collect::<Vec<_>>() {
        let atomic = term_a.atomic_not_equal(linear.bound + value);
        consistent &= state.apply(&atomic);
    }

    if consistent {
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
    _: &Fact,
    constraint: &Constraint,
    state: VariableState<Atomic>,
) -> Result<(), InvalidInference> {
    let Constraint::AllDifferent(AllDifferent { variables }) = constraint else {
        return Err(InvalidInference::ConstraintLabelMismatch);
    };

    let mut values = BTreeSet::new();
    for variable in variables {
        let Some(value) = variable.induced_fixed_value(&state) else {
            continue;
        };

        if !values.insert(value) {
            return Ok(());
        }
    }

    Err(InvalidInference::Unsound)
}
