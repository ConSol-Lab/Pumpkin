use std::fmt::Debug;
use std::rc::Rc;

use pumpkin_core::variables::IntegerVariable;

use super::Task;

/// Structures used for storing the data related to resource profiles;
/// A [`ResourceProfile`] represents a rectangle where the height is the cumulative mandatory
/// resource usage of the [`profile tasks`][ResourceProfile::profile_tasks]
#[derive(Debug, Clone)]
pub(crate) struct ResourceProfile<Var> {
    /// The start time of the [`ResourceProfile`] (inclusive)
    pub(crate) start: i32,
    /// The end time of the [`ResourceProfile`] (inclusive)
    pub(crate) end: i32,
    /// The IDs of the tasks which are part of the profile
    pub(crate) profile_tasks: Vec<Rc<Task<Var>>>,
    /// The amount of cumulative resource usage of all [`profile
    /// tasks`][ResourceProfile::profile_tasks] (i.e. the height of the rectangle)
    pub(crate) height: i32,
}

impl<Var: IntegerVariable + 'static> ResourceProfile<Var> {
    pub(crate) fn default(time: i32) -> ResourceProfile<Var> {
        ResourceProfile {
            start: time,
            end: time,
            profile_tasks: Vec::new(),
            height: 0,
        }
    }
}
