use std::fmt::Debug;
use std::rc::Rc;

use super::Task;
use crate::variables::IntegerVariable;

/// Structures used for storing the data related to resource profiles;
/// A [`ResourceProfile`] represents a rectangle where the height is the cumulative mandatory
/// resource usage of the [`profile tasks`][ResourceProfile::profile_tasks]
#[derive(Clone, Debug)]
pub(crate) struct ResourceProfile<Var, PVar, RVar> {
    /// The start time of the [`ResourceProfile`] (inclusive)
    pub(crate) start: i32,
    /// The end time of the [`ResourceProfile`] (inclusive)
    pub(crate) end: i32,
    /// The IDs of the tasks which are part of the profile
    pub(crate) profile_tasks: Vec<Rc<Task<Var, PVar, RVar>>>,
    /// The amount of cumulative resource usage of all [`profile
    /// tasks`][ResourceProfile::profile_tasks] (i.e. the height of the rectangle)
    pub(crate) height: i32,
}

impl<
        Var: IntegerVariable + 'static,
        PVar: IntegerVariable + 'static,
        RVar: IntegerVariable + 'static,
    > ResourceProfile<Var, PVar, RVar>
{
    pub(crate) fn default(time: i32) -> ResourceProfile<Var, PVar, RVar> {
        ResourceProfile {
            start: time,
            end: time,
            profile_tasks: Vec::new(),
            height: 0,
        }
    }
}
