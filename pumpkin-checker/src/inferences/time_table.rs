use std::collections::BTreeMap;

use pumpkin_checking::CheckerVariable;
use pumpkin_checking::VariableState;

use super::Fact;
use crate::inferences::InvalidInference;
use crate::model::Atomic;
use crate::model::Constraint;

/// Verifies a `time_table` inference for the cumulative constraint.
///
/// The premises and negation of the consequent should lead to an overflow of the resource
/// capacity.
pub(crate) fn verify_time_table(
    _: &Fact,
    constraint: &Constraint,
    state: VariableState<Atomic>,
) -> Result<(), InvalidInference> {
    let Constraint::Cumulative(cumulative) = constraint else {
        return Err(InvalidInference::ConstraintLabelMismatch);
    };

    // The profile is a key-value store. The keys correspond to time-points, and the values to the
    // relative change in resource consumption. A BTreeMap is used to maintain a sorted order of
    // the time points.
    let mut profile = BTreeMap::new();

    for task in cumulative.tasks.iter() {
        let lst = task.start_time.induced_upper_bound(&state);
        let ect = task.start_time.induced_lower_bound(&state) + task.duration;

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
