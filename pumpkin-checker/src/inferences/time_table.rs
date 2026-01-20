use pumpkin_checking::InferenceChecker;
use pumpkin_checking::VariableState;
use pumpkin_propagators::cumulative::time_table::CheckerTask;
use pumpkin_propagators::cumulative::time_table::TimeTableChecker;

use super::Fact;
use crate::inferences::InvalidInference;
use crate::model::Atomic;
use crate::model::Constraint;

/// Verifies a `time_table` inference for the cumulative constraint.
///
/// The premises and negation of the consequent should lead to an overflow of the resource
/// capacity.
pub(crate) fn verify_time_table(
    fact: &Fact,
    constraint: &Constraint,
    state: VariableState<Atomic>,
) -> Result<(), InvalidInference> {
    let Constraint::Cumulative(cumulative) = constraint else {
        return Err(InvalidInference::ConstraintLabelMismatch);
    };

    let checker = TimeTableChecker {
        tasks: cumulative
            .tasks
            .iter()
            .map(|task| CheckerTask {
                start_time: task.start_time.clone(),
                resource_usage: task.resource_usage,
                duration: task.duration,
            })
            .collect(),
        capacity: cumulative.capacity,
    };

    if checker.check(state, &fact.premises, fact.consequent.as_ref()) {
        Ok(())
    } else {
        Err(InvalidInference::Unsound)
    }
}
