//! Stores structures related for the Cumulative constraint such as the [`Task`]s or the
//! [`CumulativeParameters`].
use std::hash::Hash;
use std::rc::Rc;

use crate::engine::propagation::local_id::LocalId;
use crate::engine::variables::IntegerVariable;
use crate::options::CumulativeOptions;

/// Structure which stores the variables related to a task; for now, only the start times are
/// assumed to be variable
#[derive(Debug)]
pub(crate) struct Task<Var> {
    /// The variable representing the start time of a task
    pub(crate) start_variable: Var,
    /// The processing time of the `start_variable` (also referred to as duration of a task)
    pub(crate) processing_time: i32,
    /// How much of the resource the given task uses during its non-preemptive execution
    pub(crate) resource_usage: i32,
    /// The [`LocalId`] of the task
    pub(crate) id: LocalId,
}

impl<Var: IntegerVariable + 'static> Task<Var> {
    pub(crate) fn get_id(task: &Rc<Task<Var>>) -> usize {
        task.id.unpack() as usize
    }
}

impl<Var: IntegerVariable + 'static> Hash for Task<Var> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

impl<Var: IntegerVariable + 'static> PartialEq for Task<Var> {
    fn eq(&self, other: &Self) -> bool {
        self.id.unpack() == other.id.unpack()
    }
}

impl<Var: IntegerVariable + 'static> Eq for Task<Var> {}

/// The task which is passed as argument
#[derive(Clone, Debug)]
pub(crate) struct ArgTask<Var> {
    /// The [`IntegerVariable`] representing the start time of a task
    pub(crate) start_time: Var,
    /// The processing time of the [`start_time`][ArgTask::start_time] (also referred to as
    /// duration of a task)
    pub(crate) processing_time: i32,
    /// How much of the resource the given task uses during its non-preemptive execution
    pub(crate) resource_usage: i32,
}

/// Stores the information of an updated task; for example in the context of
/// [`TimeTablePerPointPropagator`] this is a task who's mandatory part has changed.
#[derive(Debug)]
pub(crate) struct UpdatedTaskInfo<Var> {
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

/// Holds the data for the cumulative constraint; more specifically it holds:
/// - The tasks
/// - The capacity of the resource
/// - The known bounds
/// - The values which have been updated since the previous propagation
#[derive(Debug)]
pub(crate) struct CumulativeParameters<Var> {
    /// The Set of [`Task`]s; for each [`Task`], the [`Task::id`] is assumed to correspond to its
    /// index in this [`Vec`]; this is stored as a [`Box`] of [`Rc`]'s to accomodate the
    /// sharing of the tasks
    pub(crate) tasks: Box<[Rc<Task<Var>>]>,
    /// The capacity of the resource (i.e. how much resource consumption can be maximally
    /// accomodated at each time point)
    pub(crate) capacity: i32,
    /// The current known bounds of the different [tasks][CumulativeParameters::tasks]; stored as
    /// (lower bound, upper bound)
    ///
    /// `bounds[i]` represents the currently known bounds of task i
    pub(crate) bounds: Vec<(i32, i32)>,
    /// The [`Task`]s which have been updated since the last round of propagation, this structure
    /// is updated by the (incremental) propagator
    pub(crate) updated: Vec<UpdatedTaskInfo<Var>>,
    /// The [`CumulativeOptions`] which influence the behaviour of the cumulative propagator(s).
    pub(crate) options: CumulativeOptions,
}

impl<Var: IntegerVariable + 'static> CumulativeParameters<Var> {
    pub(crate) fn new(
        tasks: Vec<Task<Var>>,
        capacity: i32,
        options: CumulativeOptions,
    ) -> CumulativeParameters<Var> {
        let tasks = tasks
            .into_iter()
            .map(Rc::new)
            .collect::<Vec<_>>()
            .into_boxed_slice();
        CumulativeParameters {
            tasks: tasks.clone(),
            capacity,
            bounds: Vec::new(),
            updated: Vec::new(),
            options,
        }
    }
}
