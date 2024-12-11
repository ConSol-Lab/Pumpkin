use std::rc::Rc;

use super::Task;
use crate::variables::IntegerVariable;

/// Stores the information of an updated task; for example in the context of
/// [`TimeTablePerPointPropagator`] this is a task whose mandatory part has changed.
#[derive(Debug, Clone)]
pub(crate) struct UpdatedTaskInfo<Var: IntegerVariable> {
    /// The task which has been updated (where "updated" is according to some context-dependent
    /// definition)
    pub(crate) task: Rc<Task<Var>>,
    /// The lower-bound of the [`Task`] before the update
    pub(crate) old_lower_bound: i32,
    /// The upper-bound of the [`Task`] before the update
    pub(crate) old_upper_bound: i32,
    /// The lower-bound of the [`Task`] after the update
    pub(crate) new_lower_bound: i32,
    /// The upper-bound of the [`Task`] after the update
    pub(crate) new_upper_bound: i32,
}
