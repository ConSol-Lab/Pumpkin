use implementation::propagators::cumulative::CumulativeChecker;
use implementation::propagators::cumulative::Task;
use pumpkin_checking::InferenceChecker;
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
    fact: &Fact,
    constraint: &Constraint,
    state: VariableState<Atomic>,
) -> Result<(), InvalidInference> {
    let Constraint::Cumulative(cumulative) = constraint else {
        return Err(InvalidInference::ConstraintLabelMismatch);
    };

    let checker = CumulativeChecker {
        tasks: cumulative
            .tasks
            .iter()
            .map(|task| Task {
                start_time: task.start_time.clone(),
                resource_usage: task
                    .resource_usage
                    .try_into()
                    .expect("Expected resource usage to be unsigned"),
                duration: task
                    .duration
                    .try_into()
                    .expect("Expected duration to be unsigned"),
            })
            .collect(),
        capacity: cumulative
            .capacity
            .try_into()
            .expect("Expected capacity to be unsigned"),
    };

    if checker.check(state, &fact.premises, fact.consequent.as_ref()) {
        Ok(())
    } else {
        Err(InvalidInference::Unsound)
    }
}
