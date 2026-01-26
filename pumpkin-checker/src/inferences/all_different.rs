use implementation::propagators::all_different::AllDifferentChecker;
use pumpkin_checking::InferenceChecker;
use pumpkin_checking::VariableState;

use super::Fact;
use crate::inferences::InvalidInference;
use crate::model::Atomic;
use crate::model::Constraint;

/// Verify an `all_different` inference.
///
/// The checker tests that the premises and the negation of the consequent form a hall-set. If that
/// is the case, the inference is accepted. Otherwise, the inference is rejected.
///
/// The checker will reject inferences with redundant atomic constraints.
pub(crate) fn verify_all_different(
    fact: &Fact,
    constraint: &Constraint,
    state: VariableState<Atomic>,
) -> Result<(), InvalidInference> {
    // This checker takes the union of the domains of the variables in the constraint. If there
    // are fewer values in the union of the domain than there are variables, then there is a
    // conflict and the inference is valid.

    let Constraint::AllDifferent(all_different) = constraint else {
        return Err(InvalidInference::ConstraintLabelMismatch);
    };

    let checker = AllDifferentChecker {
        x: all_different.variables.clone(),
    };

    if checker.check(state, &fact.premises, fact.consequent.as_ref()) {
        Ok(())
    } else {
        Err(InvalidInference::Unsound)
    }
}
