use std::collections::BTreeMap;

use super::Fact;
use crate::inferences::InvalidInference;
use crate::model::Constraint;
use crate::state::VariableState;

/// Verifies a `time_table` inference for the cumulative constraint.
///
/// The premises and negation of the conclusion should lead to an overflow of the resource
/// capacity.
pub(crate) fn verify_time_table(
    fact: &Fact,
    constraint: &Constraint,
) -> Result<(), InvalidInference> {
    let Constraint::Cumulative(cumulative) = constraint else {
        return Err(InvalidInference::ConstraintLabelMismatch);
    };

    let variable_state = VariableState::prepare_for_conflict_check(fact)
        .ok_or(InvalidInference::InconsistentPremises)?;

    // The profile is a key-value store. The keys correspond to time-points, and the values to the
    // relative change in resource consumption. A BTreeMap is used to maintain a sorted order of
    // the time points.
    let mut profile = BTreeMap::new();

    for task in cumulative.tasks.iter() {
        let lst = variable_state.upper_bound(&task.start_time);
        let ect = variable_state.lower_bound(&task.start_time) + task.duration;

        if ect <= lst {
            *profile.entry(ect).or_insert(0) += task.resource_usage;
            *profile.entry(lst).or_insert(0) -= task.resource_usage;
        }
    }

    let mut usage = 0;
    for delta in profile.values() {
        usage += delta;

        if usage > cumulative.capacity {
            return Ok(());
        }
    }

    Err(InvalidInference::Unsound)
}
