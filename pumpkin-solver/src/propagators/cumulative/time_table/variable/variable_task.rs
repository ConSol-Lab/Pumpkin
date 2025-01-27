use crate::engine::propagation::LocalId;

/// Structure which stores the variables related to a task; for now, only the start times are
/// assumed to be variable
#[derive(Debug)]
pub(crate) struct VariableTask<Var> {
    /// The variable representing the start time of a task
    pub(crate) start_variable: Var,
    /// The processing time of the `start_variable` (also referred to as duration of a task)
    pub(crate) processing_time: Var,
    /// How much of the resource the given task uses during its non-preemptive execution
    pub(crate) resource_usage: Var,
    /// The [`LocalId`] of the task
    pub(crate) id: LocalId,
}
