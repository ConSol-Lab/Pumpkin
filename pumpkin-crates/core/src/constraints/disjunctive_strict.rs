use std::fmt::Debug;

use crate::constraints::Constraint;
use crate::proof::ConstraintTag;
use crate::propagators::disjunctive_propagator::DisjunctivePropagator;
use crate::propagators::disjunctive_task::ArgDisjunctiveTask;
use crate::pumpkin_assert_simple;
use crate::variables::IntegerVariable;

pub fn disjunctive_strict<StartTimes, Durations>(
    start_times: StartTimes,
    durations: Durations,
    constraint_tag: ConstraintTag,
) -> impl Constraint
where
    StartTimes: IntoIterator,
    StartTimes::Item: IntegerVariable + Debug + 'static,
    StartTimes::IntoIter: ExactSizeIterator,
    Durations: IntoIterator<Item = i32>,
    Durations::IntoIter: ExactSizeIterator,
{
    let start_times = start_times.into_iter();
    let durations = durations.into_iter();

    pumpkin_assert_simple!(
        start_times.len() == durations.len(),
        "The number of start variables and durations should be the same!"
    );

    DisjunctivePropagator::new(
        start_times
            .zip(durations)
            .map(|(start_time, duration)| ArgDisjunctiveTask {
                start_time,
                processing_time: duration,
            })
            .collect::<Vec<_>>(),
        constraint_tag,
    )
}
