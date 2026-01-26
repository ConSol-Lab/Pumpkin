use std::collections::BTreeSet;

use pumpkin_checking::CheckerVariable;
use pumpkin_checking::InferenceChecker;
use pumpkin_checking::VariableState;
use pumpkin_propagators::arithmetic::BinaryEqualsChecker;

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
    fact: &Fact,
    constraint: &Constraint,
    state: VariableState<Atomic>,
) -> Result<(), InvalidInference> {
    // To check this inference we expect the intersection of both domains to be empty.

    let Constraint::LinearEq(linear) = constraint else {
        return Err(InvalidInference::ConstraintLabelMismatch);
    };

    // For now, this inference only works for constraints over two variables.
    if linear.terms.len() != 2 {
        return Err(InvalidInference::Unsound);
    }

    let lhs = linear.terms[0].clone();
    let rhs = linear.terms[1].clone();

    let checker = BinaryEqualsChecker { lhs, rhs };

    if checker.check(state, &fact.premises, fact.consequent.as_ref()) {
        Ok(())
    } else {
        Err(InvalidInference::Unsound)
    }
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
