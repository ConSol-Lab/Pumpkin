use std::fmt::Debug;

use super::Constraint;
use crate::propagators::disjunctive_propagator::Disjunctive;
use crate::propagators::disjunctive_task::ArgDisjunctiveTask;
use crate::variables::IntegerVariable;

/// Creates the [Disjunctive](https://sofdem.github.io/gccat/gccat/Cdisjunctive.html) [`Constraint`].
///
/// This constraint ensures that at no point in time the provided task can overlap. This can be
/// seen as a special case of the `cumulative` constraint with capacity 1.
///
/// The implementation uses the edge-finding as described in \[1\].
///
/// The length of `start_times` and `durations` should be the same; if
/// this is not the case then this method will panic.
///
/// # Bibliography
/// \[1\] P. Vilím, ‘Global constraints in scheduling’, 2007.
pub fn disjunctive<StartTimes, Durations>(
    start_times: StartTimes,
    durations: Durations,
) -> impl Constraint
where
    StartTimes: IntoIterator,
    StartTimes::Item: IntegerVariable + Debug + 'static,
    StartTimes::IntoIter: ExactSizeIterator,
    Durations: IntoIterator<Item = i32>,
    Durations::IntoIter: ExactSizeIterator,
{
    Disjunctive::new(
        start_times
            .into_iter()
            .zip(durations)
            .map(|(start_time, duration)| ArgDisjunctiveTask {
                start_variable: start_time,
                processing_time: duration,
            }),
    )
}
