use std::borrow::Cow;
use std::marker::PhantomData;

use pumpkin_checking::ConflictChecker;
use pumpkin_checking::checkers::CheckerTask;
use pumpkin_checking::checkers::TimeTableChecker;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::propagation::ConflictRule;
use pumpkin_core::variables::IntegerVariable;

use super::CumulativeDescription;

/// The inference rule of the propagator.
#[derive(Clone, Copy, Debug)]
pub struct TimeTableRule<Var>(PhantomData<Var>);

impl<Var: IntegerVariable + 'static> ConflictRule for TimeTableRule<Var> {
    type Description = CumulativeDescription<Var>;

    fn name() -> Cow<'static, str> {
        Cow::Borrowed("time_table")
    }

    fn create_conflict_checker(
        constraint_description: &CumulativeDescription<Var>,
    ) -> impl ConflictChecker<Predicate> + 'static {
        TimeTableChecker {
            tasks: constraint_description
                .tasks
                .iter()
                .map(|task| CheckerTask {
                    start_time: task.start_time.clone(),
                    processing_time: task.processing_time,
                    resource_usage: task.resource_usage,
                })
                .collect(),
            capacity: constraint_description.capacity,
        }
    }
}
