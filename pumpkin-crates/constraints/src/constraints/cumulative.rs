use std::fmt::Debug;

use implementation::propagators::cumulative::CumulativeConstructor;
use implementation::propagators::cumulative::Task;
use pumpkin_core::constraints::Constraint;
use pumpkin_core::proof::ConstraintTag;
use pumpkin_core::variables::IntegerVariable;

/// Creates the [Cumulative](https://sofdem.github.io/gccat/gccat/Ccumulative.html) [`Constraint`].
pub fn cumulative<Var: IntegerVariable + Debug + 'static>(
    tasks: impl IntoIterator<Item = Task<Var>>,
    resource_capacity: i32,
    constraint_tag: ConstraintTag,
    conflict_detection_only: bool,
) -> impl Constraint {
    CumulativeConstructor {
        tasks: tasks.into_iter().collect(),
        capacity: resource_capacity
            .try_into()
            .expect("Expected u32 for capacity"),
        constraint_tag,
        conflict_detection_only,
    }
}
