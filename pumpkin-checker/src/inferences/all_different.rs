use pumpkin_checking::CheckerVariable;
use pumpkin_checking::Union;
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
    _: &Fact,
    constraint: &Constraint,
    state: VariableState<Atomic>,
) -> Result<(), InvalidInference> {
    // This checker takes the union of the domains of the variables in the constraint. If there
    // are fewer values in the union of the domain than there are variables, then there is a
    // conflict and the inference is valid.

    let Constraint::AllDifferent(all_different) = constraint else {
        return Err(InvalidInference::ConstraintLabelMismatch);
    };

    let variables = all_different
        .variables
        .iter()
        .filter(|variable| variable.iter_induced_domain(&state).is_some())
        .collect::<Vec<_>>();

    // Collect all values present in at least one of the domains.
    let mut union = Union::empty();
    for &variable in &variables {
        union.add(&state, variable);
    }

    if let Some(union_size) = union.size()
        && union_size < variables.len()
    {
        Ok(())
    } else {
        Err(InvalidInference::Unsound)
    }
}
