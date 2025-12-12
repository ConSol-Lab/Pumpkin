use crate::inferences::Fact;
use crate::inferences::InvalidInference;
use crate::model::Constraint;
use crate::state::VariableState;

/// Verifies a `nogood` inference.
///
/// This inference is used to rewrite a nogood `L /\ p -> false` to `L -> not p`.
pub(crate) fn verify_nogood(fact: &Fact, constraint: &Constraint) -> Result<(), InvalidInference> {
    let Constraint::Nogood(nogood) = constraint else {
        return Err(InvalidInference::ConstraintLabelMismatch);
    };

    let variable_state = VariableState::prepare_for_conflict_check(fact)
        .ok_or(InvalidInference::InconsistentPremises)?;

    let is_implied_by_nogood = nogood.iter().all(|atomic| variable_state.is_true(atomic));

    if is_implied_by_nogood {
        Ok(())
    } else {
        Err(InvalidInference::Unsound)
    }
}
