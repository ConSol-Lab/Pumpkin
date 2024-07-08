//! Stores structures related for the Cumulative constraint such as the [`Task`]s or the
//! [`CumulativeParameters`].
use std::hash::Hash;
use std::marker::PhantomData;
use std::rc::Rc;

use crate::engine::propagation::local_id::LocalId;
use crate::engine::variables::IntegerVariable;
use crate::propagators::TimeTableOverIntervalPropagator;
use crate::propagators::TimeTablePerPointIncrementalPropagator;
use crate::propagators::TimeTablePerPointPropagator;

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
pub struct ArgTask<Var> {
    /// The [`IntegerVariable`] representing the start time of a task
    pub start_time: Var,
    /// The processing time of the [`start_time`][ArgTask::start_time] (also referred to as
    /// duration of a task)
    pub processing_time: i32,
    /// How much of the resource the given task uses during its non-preemptive execution
    pub resource_usage: i32,
}

/// The arguments which are required to create the constraint/propagators
#[derive(Debug, Clone)]
pub struct CumulativeConstructor<Var, T> {
    /// A box containing all of the [`ArgTask`]s
    pub tasks: Box<[ArgTask<Var>]>,
    /// The capacity of the resource
    pub capacity: i32,
    /// We use [`PhantomData`] to differentiate between the different types of propagators;
    /// without this field we would need to create a new argument struct for each cumulative
    /// propagator
    propagator_type: PhantomData<T>,
    /// Specifies whether it is allowed to create holes in the domain; if this parameter is set to
    /// false then it will only adjust the bounds when appropriate rather than removing values from
    /// the domain
    pub allow_holes_in_domain: bool,
}

impl<Var, T> CumulativeConstructor<Var, T> {
    pub fn new(tasks: Box<[ArgTask<Var>]>, capacity: i32, allow_holes_in_domain: bool) -> Self {
        CumulativeConstructor {
            tasks,
            capacity,
            propagator_type: PhantomData,
            allow_holes_in_domain,
        }
    }
}

/// An alias used for calling the [`CumulativeConstructor::new`] method with the concrete propagator
/// type of [`TimeTablePerPointPropagator`]; this is used to prevent creating a different `new`
/// method for each type `T`
pub type TimeTablePerPoint<Var> = CumulativeConstructor<Var, TimeTablePerPointPropagator<Var>>;

/// An alias used for calling the [`CumulativeConstructor::new`] method with the concrete propagator
/// type of [`TimeTablePerPointIncrementalPropagator`]; this is used to prevent creating a different
/// `new` method for each type `T`
pub type TimeTablePerPointIncremental<Var> =
    CumulativeConstructor<Var, TimeTablePerPointIncrementalPropagator<Var>>;

/// An alias used for calling the [`CumulativeConstructor::new`] method with the concrete propagator
/// type of [`TimeTableOverIntervalPropagator`]; this is used to prevent creating a different
/// `new` method for each type `T`
pub type TimeTableOverInterval<Var> =
    CumulativeConstructor<Var, TimeTableOverIntervalPropagator<Var>>;

/// Stores the information of an updated task; for example in the context of
/// [`TimeTablePerPointPropagator`] this is a task who's mandatory part has changed.
#[derive(Debug)]
#[allow(dead_code)] // Temporary until the structure is used
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
pub struct CumulativeParameters<Var> {
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
    /// Specifies whether it is allowed to create holes in the domain; if this parameter is set to
    /// false then it will only adjust the bounds when appropriate rather than removing values from
    /// the domain
    pub(crate) allow_holes_in_domain: bool,
}

impl<Var: IntegerVariable + 'static> CumulativeParameters<Var> {
    pub(crate) fn new(
        tasks: Vec<Task<Var>>,
        capacity: i32,
        allow_holes_in_domain: bool,
    ) -> CumulativeParameters<Var> {
        CumulativeParameters {
            tasks: tasks
                .into_iter()
                .map(Rc::new)
                .collect::<Vec<_>>()
                .into_boxed_slice(),
            capacity,
            bounds: Vec::new(),
            updated: Vec::new(),
            allow_holes_in_domain,
        }
    }
}
