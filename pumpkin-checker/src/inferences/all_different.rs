use std::collections::HashSet;

use super::Fact;
use crate::inferences::InvalidInference;
use crate::model::Constraint;
use crate::state::VariableState;

/// Verify an `all_different` inference.
///
/// The checker tests that the premises and the negation of the conclusion form a hall-set. If that
/// is the case, the inference is accepted. Otherwise, the inference is rejected.
///
/// The checker will reject inferences with redundant atomic constraints.
pub(crate) fn verify_all_different(
    fact: &Fact,
    constraint: &Constraint,
) -> Result<(), InvalidInference> {
    let Constraint::AllDifferent(all_different) = constraint else {
        return Err(InvalidInference::ConstraintLabelMismatch);
    };

    let variable_state = VariableState::prepare_for_conflict_check(fact)
        .ok_or(InvalidInference::InconsistentPremises)?;

    let union_of_domains = all_different
        .variables
        .iter()
        .filter_map(|variable| variable_state.iter_domain(variable))
        .flatten()
        .collect::<HashSet<_>>();

    let variables = all_different
        .variables
        .iter()
        .filter(|variable| variable_state.iter_domain(variable).is_some())
        .collect::<Vec<_>>();

    if union_of_domains.len() < variables.len() {
        Ok(())
    } else {
        Err(InvalidInference::Unsound)
    }
}
