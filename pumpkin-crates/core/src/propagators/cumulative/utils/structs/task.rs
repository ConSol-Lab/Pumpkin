use std::fmt::Debug;
use std::hash::Hash;
use std::rc::Rc;

use crate::engine::propagation::LocalId;
use crate::variables::IntegerVariable;

/// Structure which stores the variables related to a task; for now, only the start times are
/// assumed to be variable
#[derive(Debug)]
pub(crate) struct Task<Var, PVar, RVar> {
    /// The variable representing the start time of a task
    pub(crate) start_variable: Var,
    /// The processing time of the `start_variable` (also referred to as duration of a task)
    pub(crate) processing_time: PVar,
    /// How much of the resource the given task uses during its non-preemptive execution
    pub(crate) resource_usage: RVar,
    /// The [`LocalId`] of the task
    pub(crate) id: LocalId,
}

impl<
        Var: IntegerVariable + 'static,
        PVar: IntegerVariable + 'static,
        RVar: IntegerVariable + 'static,
    > Task<Var, PVar, RVar>
{
    pub(crate) fn get_id(task: &Rc<Task<Var, PVar, RVar>>) -> usize {
        task.id.unpack() as usize
    }
}

impl<
        Var: IntegerVariable + 'static,
        PVar: IntegerVariable + 'static,
        RVar: IntegerVariable + 'static,
    > Hash for Task<Var, PVar, RVar>
{
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

impl<
        Var: IntegerVariable + 'static,
        PVar: IntegerVariable + 'static,
        RVar: IntegerVariable + 'static,
    > PartialEq for Task<Var, PVar, RVar>
{
    fn eq(&self, other: &Self) -> bool {
        self.id.unpack() == other.id.unpack()
    }
}

impl<
        Var: IntegerVariable + 'static,
        PVar: IntegerVariable + 'static,
        RVar: IntegerVariable + 'static,
    > Eq for Task<Var, PVar, RVar>
{
}

/// The task which is passed as argument
#[derive(Clone, Debug)]
pub(crate) struct ArgTask<Var, PVar, RVar> {
    /// The [`IntegerVariable`] representing the start time of a task
    pub(crate) start_time: Var,
    /// The processing time of the [`start_time`][ArgTask::start_time] (also referred to as
    /// duration of a task)
    pub(crate) processing_time: PVar,
    /// How much of the resource the given task uses during its non-preemptive execution
    pub(crate) resource_usage: RVar,
}
