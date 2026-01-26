use std::collections::HashSet;

use pumpkin_checking::CheckerVariable;
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

    // Collect all values present in at least one of the domains.
    let union_of_domains = all_different
        .variables
        .iter()
        .filter_map(|variable| variable.iter_induced_domain(&state))
        .flatten()
        .collect::<HashSet<_>>();

    // Collect the variables mentioned in the fact. Here we ignore variables with a domain
    // equal to all integers, as they are not mentioned in the fact. Therefore they do not
    // contribute in the hall-set reasoning.
    let num_variables = all_different
        .variables
        .iter()
        .filter(|variable| variable.iter_induced_domain(&state).is_some())
        .count();

    if union_of_domains.len() < num_variables {
        Ok(())
    } else {
        Err(InvalidInference::Unsound)
    }
}
