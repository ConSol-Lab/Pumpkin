use std::fmt::Debug;

use super::Constraint;
use crate::pumpkin_assert_simple;
use crate::variables::IntegerVariable;

/// Creates the [Disjunctive](https://sofdem.github.io/gccat/gccat/Cdisjunctive.html) [`Constraint`].
///
/// This constraint ensures that at no point in time the provided task can overlap. This can be
/// seen as a special case of the `cumulative` constraint with capacity 1.
///
/// The length of `start_times` and `durations` should be the same; if
/// this is not the case then this method will panic.
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
    let start_times = start_times.into_iter().collect::<Vec<_>>();
    let durations = durations.into_iter().collect::<Vec<_>>();

    pumpkin_assert_simple!(start_times.len() == durations.len());

    todo!("Call your Disjunctive propagator here!")
    // Disjunctive::new(start_times, durations)
}
