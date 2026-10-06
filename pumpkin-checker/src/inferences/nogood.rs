use pumpkin_checking::ConflictCheck;
use pumpkin_checking::ConflictChecker;
use pumpkin_checking::VariableState;
use pumpkin_checking::checkers::NogoodChecker;

use crate::inferences::Fact;
use crate::inferences::InvalidInference;
use crate::model::Atomic;
use crate::model::Constraint;

/// Verifies a `nogood` inference.
///
/// This inference is used to rewrite a nogood `L /\ p -> false` to `L -> not p`.
pub(crate) fn verify_nogood(
    fact: &Fact,
    constraint: &Constraint,
    state: VariableState<Atomic>,
) -> Result<(), InvalidInference> {
    let Constraint::Nogood(nogood) = constraint else {
        return Err(InvalidInference::ConstraintLabelMismatch);
    };

    let checker = NogoodChecker {
        nogood: nogood.iter().cloned().collect(),
    };

    if checker.check(state, &fact.premises, fact.consequent.as_ref())
        == ConflictCheck::ConflictDetected
    {
        Ok(())
    } else {
        Err(InvalidInference::Unsound)
    }
}
