use std::fmt::Debug;

use implementation::cumulative::CumulativeConstructor;
use pumpkin_core::constraints::Constraint;
use pumpkin_core::proof::ConstraintTag;
use pumpkin_core::variables::IntegerVariable;

/// Creates the [Cumulative](https://sofdem.github.io/gccat/gccat/Ccumulative.html) [`Constraint`].
pub fn cumulative<StartTimes, Durations, ResourceRequirements>(
    start_times: StartTimes,
    durations: Durations,
    resource_requirements: ResourceRequirements,
    resource_capacity: i32,
    constraint_tag: ConstraintTag,
) -> impl Constraint
where
    StartTimes: IntoIterator,
    StartTimes::Item: IntegerVariable + Debug + 'static,
    StartTimes::IntoIter: ExactSizeIterator,
    Durations: IntoIterator<Item = i32>,
    Durations::IntoIter: ExactSizeIterator,
    ResourceRequirements: IntoIterator<Item = i32>,
    ResourceRequirements::IntoIter: ExactSizeIterator,
{
    CumulativeConstructor {
        start_times: start_times.into_iter().collect(),
        durations: durations
            .into_iter()
            .map(|duration| duration.try_into().expect("Expected u32 for duration"))
            .collect(),
        resource_usages: resource_requirements
            .into_iter()
            .map(|resource_requirement| {
                resource_requirement
                    .try_into()
                    .expect("Expected u32 for resource requirements")
            })
            .collect(),
        capacity: resource_capacity
            .try_into()
            .expect("Expected u32 for capacity"),
        constraint_tag,
    }
}
