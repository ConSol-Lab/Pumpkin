use std::rc::Rc;

use crate::propagators::Task;

/// Structures used for storing the data related to resource profiles;
/// A [`PointResourceProfile`] represents a cumulative mandatory
/// resource usage of the [`profile tasks`][ResourceProfile::profile_tasks] at time point
/// [`PointResourceProfile::point`].
#[derive(Clone, Debug)]
pub(crate) struct PointResourceProfile<Var> {
    /// The start time of the [`ResourceProfile`] (inclusive)
    point: i32,
    /// The IDs of the tasks which are part of the profile
    profile_tasks: Vec<Rc<Task<Var>>>,
    /// The amount of cumulative resource usage of all [`profile
    /// tasks`][ResourceProfile::profile_tasks] (i.e. the height of the rectangle)
    height: i32,
}
