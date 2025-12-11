use std::collections::HashSet;

use super::Fact;
use crate::inferences::InvalidInference;
use crate::model::Constraint;
use crate::state::VariableState;

/// Verify an `all_different` inference.
///
/// The checker tests that the premises and the negation of the consequent form a hall-set. If that
/// is the case, the inference is accepted. Otherwise, the inference is rejected.
///
/// The checker will reject inferences with redundant atomic constraints.
pub(crate) fn verify_all_different(
    fact: &Fact,
    constraint: &Constraint,
) -> Result<(), InvalidInference> {
    // This checker takes the union of the domains of the variables in the constraint. If there
    // are fewer values in the union of the domain than there are variables, then there is a
    // conflict and the inference is valid.

    let Constraint::AllDifferent(all_different) = constraint else {
        return Err(InvalidInference::ConstraintLabelMismatch);
    };

    let variable_state = VariableState::prepare_for_conflict_check(fact)
        .ok_or(InvalidInference::InconsistentPremises)?;

    // Collect all values present in at least one of the domains.
    let union_of_domains = all_different
        .variables
        .iter()
        .filter_map(|variable| variable_state.iter_domain(variable))
        .flatten()
        .collect::<HashSet<_>>();

    // Collect the variables mentioned in the fact. Here we ignore variables with a domain
    // equal to all integers, as they are not mentioned in the fact. Therefore they do not
    // contribute in the hall-set reasoning.
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
