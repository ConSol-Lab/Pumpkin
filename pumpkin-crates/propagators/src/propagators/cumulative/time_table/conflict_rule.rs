use std::borrow::Cow;
use std::marker::PhantomData;

use pumpkin_checking::InferenceChecker;
use pumpkin_checking::RetentionChecker;
use pumpkin_checking::checkers::CheckerTask;
use pumpkin_checking::checkers::TimeTableChecker;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::propagation::ConflictRule;
use pumpkin_core::variables::IntegerVariable;

use super::CumulativeDescription;

/// The time-table rule of the cumulative constraint, which all the time-table propagators
/// implement.
#[derive(Clone, Copy, Debug)]
pub struct TimeTableRule<Var>(PhantomData<Var>);

impl<Var: IntegerVariable + 'static> ConflictRule for TimeTableRule<Var> {
    type Description = CumulativeDescription<Var>;

    fn name() -> Cow<'static, str> {
        Cow::Borrowed("time_table")
    }

    fn create_inference_checker(
        description: &CumulativeDescription<Var>,
    ) -> impl InferenceChecker<Predicate> + 'static {
        Self::checker(description)
    }

    fn create_retention_checker(
        description: &CumulativeDescription<Var>,
    ) -> impl RetentionChecker<Predicate> + 'static {
        Self::checker(description)
    }
}

impl<Var: IntegerVariable + 'static> TimeTableRule<Var> {
    fn checker(description: &CumulativeDescription<Var>) -> TimeTableChecker<Var> {
        TimeTableChecker {
            tasks: description
                .tasks
                .iter()
                .map(|task| CheckerTask {
                    start_time: task.start_time.clone(),
                    processing_time: task.processing_time,
                    resource_usage: task.resource_usage,
                })
                .collect(),
            capacity: description.capacity,
        }
    }
}
