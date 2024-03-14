//! Stores structures related for the Cumulative constraint such as the [`Task`]s or the
//! [`CumulativeParameters`].
use std::hash::Hash;
use std::marker::PhantomData;
use std::rc::Rc;

use crate::basic_types::variables::IntVar;
use crate::engine::propagation::local_id::LocalId;
use crate::engine::propagation::propagator_variable::PropagatorVariable;
use crate::propagators::TimeTableOverIntervalIncrementalPropagator;
use crate::propagators::TimeTableOverIntervalPropagator;
use crate::propagators::TimeTablePerPointIncrementalPropagator;
use crate::propagators::TimeTablePerPointPropagator;

/// Structure which stores the variables related to a task; for now, only the start times are
/// assumed to be variable
#[derive(Debug)]
pub struct Task<Var> {
    /// The [`PropagatorVariable`] representing the start time of a task
    pub start_variable: PropagatorVariable<Var>,
    /// The processing time of the `start_variable` (also referred to as duration of a task)
    pub processing_time: i32,
    /// How much of the resource the given task uses during its non-preemptive execution
    pub resource_usage: i32,
    /// The [`LocalId`] of the task
    pub id: LocalId,
}

impl<Var: IntVar + 'static> Task<Var> {
    pub fn get_id(task: &Rc<Task<Var>>) -> usize {
        task.id.unpack() as usize
    }
}

impl<Var: IntVar + 'static> Hash for Task<Var> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

impl<Var: IntVar + 'static> PartialEq for Task<Var> {
    fn eq(&self, other: &Self) -> bool {
        self.id.unpack() == other.id.unpack()
    }
}

impl<Var: IntVar + 'static> Eq for Task<Var> {}

/// The task which is passed as argument
#[derive(Clone, Debug)]
pub struct ArgTask<Var> {
    /// The [`IntVar`] representing the start time of a task
    pub start_time: Var,
    /// The processing time of the [`start_time`][ArgTask::start_time] (also referred to as
    /// duration of a task)
    pub processing_time: i32,
    /// How much of the resource the given task uses during its non-preemptive execution
    pub resource_usage: i32,
}

/// The arguments which are required to create the constraint/propagators
#[derive(Debug, Clone)]
pub struct CumulativeArgs<Var, T> {
    /// A box containing all of the [`ArgTask`]s
    pub tasks: Box<[ArgTask<Var>]>,
    /// The capacity of the resource
    pub capacity: i32,
    /// We use [`PhantomData`] to differentiate between the different types of propagators;
    /// without this field we would need to create a new argument struct for each cumulative
    /// propagator
    propagator_type: PhantomData<T>,
}

impl<Var, T> CumulativeArgs<Var, T> {
    pub fn new(tasks: Box<[ArgTask<Var>]>, capacity: i32) -> Self {
        CumulativeArgs {
            tasks,
            capacity,
            propagator_type: PhantomData,
        }
    }
}

/// An alias used for calling the [`CumulativeArgs::new`] method with the concrete propagator type
/// of [`TimeTablePerPointPropagator`]; this is used to prevent creating a different `new` method
/// for each type `T`
pub type TimeTablePerPoint<Var> = CumulativeArgs<Var, TimeTablePerPointPropagator<Var>>;

/// An alias used for calling the [`CumulativeArgs::new`] method with the concrete propagator type
/// of [`TimeTablePerPointIncrementalPropagator`]; this is used to prevent creating a different
/// `new` method for each type `T`
pub type TimeTablePerPointIncremental<Var> =
    CumulativeArgs<Var, TimeTablePerPointIncrementalPropagator<Var>>;

/// An alias used for calling the [`CumulativeArgs::new`] method with the concrete propagator type
/// of [`TimeTableOverIntervalPropagator`]; this is used to prevent creating a different
/// `new` method for each type `T`
pub type TimeTableOverInterval<Var> = CumulativeArgs<Var, TimeTableOverIntervalPropagator<Var>>;

/// An alias used for calling the [`CumulativeArgs::new`] method with the concrete propagator type
/// of [`TimeTableOverIntervalIncrementalPropagator`]; this is used to prevent creating a different
/// `new` method for each type `T`
pub type TimeTableOverIntervalIncremental<Var> =
    CumulativeArgs<Var, TimeTableOverIntervalIncrementalPropagator<Var>>;

/// Stores the information of an updated task; for example in the context of
/// [`TimeTablePerPointPropagator`] this is a task who's mandatory part has changed.
#[derive(Debug)]
pub struct UpdatedTaskInfo<Var> {
    /// The task which has been updated (where "updated" is according to some context-dependent
    /// definition)
    pub task: Rc<Task<Var>>,
    /// The lower-bound of the [`Task`] before the update
    pub old_lower_bound: i32,
    /// The upper-bound of the [`Task`] before the update
    pub old_upper_bound: i32,
    /// The lower-bound of the [`Task`] after the update
    pub new_lower_bound: i32,
    /// The upper-bound of the [`Task`] after the update
    pub new_upper_bound: i32,
}

/// Holds the data for the cumulative constraint; more specifically it holds:
/// - The tasks
/// - The capacity of the resource
/// - The known bounds
/// - The values which have been updated since the previous propagation
/// - The horizon
#[derive(Debug)]
pub struct CumulativeParameters<Var> {
    /// The Set of [`Task`]s; for each [`Task`], the [`LocalId`] is assumed to correspond to its
    /// index in this [`Vec`]; this is stored as a [`Box`] of [`Rc`]'s to accomodate the
    /// sharing of the tasks
    pub tasks: Box<[Rc<Task<Var>>]>,
    /// The capacity of the resource (i.e. how much resource consumption can be maximally
    /// accomodated at each time point)
    pub capacity: i32,
    /// The current known bounds of the different [tasks][CumulativeParameters::tasks]; stored as
    /// (lower bound, upper bound)
    ///
    /// `bounds[i]` represents the currently known bounds of task i
    pub bounds: Vec<(i32, i32)>,
    /// The [`Task`]s which have been updated since the last round of propagation, this structure
    /// is updated by the (incremental) propagator
    pub updated: Vec<UpdatedTaskInfo<Var>>,
}

impl<Var: IntVar + 'static> CumulativeParameters<Var> {
    pub fn new(tasks: Vec<Task<Var>>, capacity: i32) -> CumulativeParameters<Var> {
        CumulativeParameters {
            tasks: tasks
                .into_iter()
                .map(Rc::new)
                .collect::<Vec<_>>()
                .into_boxed_slice(),
            capacity,
            bounds: Vec::new(),
            updated: Vec::new(),
        }
    }
}
