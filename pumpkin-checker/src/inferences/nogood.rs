use std::ops::Deref;

use pumpkin_checking::InferenceChecker;
use pumpkin_checking::VariableState;
use pumpkin_core::propagators::nogoods::NogoodChecker;

use crate::inferences::Fact;
use crate::inferences::InvalidInference;
use crate::model::Atomic;
use crate::model::Constraint;

/// Verifies a `nogood` inference.
///
/// This inference is used to rewrite a nogood `L /\ p -> false` to `L -> not p`.
pub(crate) fn verify_nogood(
    _: &Fact,
    constraint: &Constraint,
    state: VariableState<Atomic>,
) -> Result<(), InvalidInference> {
    let Constraint::Nogood(nogood) = constraint else {
        return Err(InvalidInference::ConstraintLabelMismatch);
    };

    let checker = NogoodChecker {
        nogood: nogood.deref().into(),
    };

    if checker.check(state) {
        Ok(())
    } else {
        Err(InvalidInference::Unsound)
    }
}
