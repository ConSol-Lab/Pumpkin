use crate::propagators::{
    TimeTableOverIntervalIncrementalProp, TimeTableOverIntervalProp,
    TimeTablePerPointIncrementalProp,
};
use crate::{
    basic_types::{variables::IntVar, PropositionalConjunction},
    engine::{LocalId, PropagatorVariable},
    propagators::TimeTablePerPointProp,
};
use std::rc::Rc;
use std::{hash::Hash, marker::PhantomData};

use super::ChangeWithBound;

#[derive(Debug)]
/// Structure which stores the variables related to a task; for now, only the start times are assumed to be variable
pub struct Task<Var> {
    /// The [PropagatorVariable] representing the start time of a task
    pub start_variable: PropagatorVariable<Var>,
    /// The processing time of the `start_variable` (also referred to as duration of a task)
    pub processing_time: i32,
    /// How much of the resource the given task uses during its non-preemptive execution
    pub resource_usage: i32,
    /// The [LocalId] of the task, this corresponds with its index into [tasks][Cumulative::tasks]
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

#[derive(Clone, Debug)]
/// The task which is passed as argument
pub struct ArgTask<Var> {
    /// The [IntVar] representing the start time of a task
    pub start_time: Var,
    /// The processing time of the `start_variable` (also referred to as duration of a task)
    pub processing_time: i32,
    /// How much of the resource the given task uses during its non-preemptive execution
    pub resource_usage: i32,
}
#[derive(Clone)]
/// The arguments which are required to create the constraint/propagators
pub struct CumulativeArgs<Var, T> {
    /// A box containing all of the ArgTasks
    pub tasks: Box<[ArgTask<Var>]>,
    /// The capacity of the resource
    pub capacity: i32,
    /// We use [PhantomData] to differentiate between the different types of propagators;
    /// without this field we would need to create a new argument struct for each cumulative propagator
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

/// An alias used for calling the [CumulativeArgs::new] method with the concrete propagator type of [TimeTablePerPointProp];
/// this is used to prevent creating a different `new` method for each type `T`
pub type TimeTablePerPoint<Var> = CumulativeArgs<Var, TimeTablePerPointProp<Var>>;
pub type TimeTablePerPointIncremental<Var> =
    CumulativeArgs<Var, TimeTablePerPointIncrementalProp<Var>>;
pub type TimeTableOverInterval<Var> = CumulativeArgs<Var, TimeTableOverIntervalProp<Var>>;
pub type TimeTableOverIntervalIncremental<Var> =
    CumulativeArgs<Var, TimeTableOverIntervalIncrementalProp<Var>>;

#[derive(Debug)]
/// Stores the information of an updated task
pub struct Updated<Var> {
    pub task: Rc<Task<Var>>,
    pub old_lower_bound: i32,
    pub old_upper_bound: i32,
    pub new_lower_bound: i32,
    pub new_upper_bound: i32,
}

/// Holds the data for the cumulative constraint;
/// the tasks, the capacity, the known bounds, the values which have been updated since the previous proapgation and the horizon
pub struct CumulativeParameters<Var> {
    /// The Set of Tasks; for each task, the [LocalId] is assumed to correspond to its index in this [Vec];
    /// this is stored as a Box of Rc's to accomodate the sharing of the tasks
    pub tasks: Box<[Rc<Task<Var>>]>,
    /// The capacity of the resource (i.e. how much resource consumption can be maximally accomodated at each time point)
    pub capacity: i32,
    /// The current known bounds of the different tasks; stored as (lower bound, upper bound)
    ///
    /// [i] represents the currently known bounds of task i
    pub bounds: Vec<(i32, i32)>,
    /// The variables which have been updated since the last round of propagation, this structure is updated by the (incremental) propagator
    pub updated: Vec<Updated<Var>>,
    /// The largest possible makespan, in this case it is assumed to be the sum of all processing times
    pub horizon: i32,
}

impl<Var: IntVar + 'static> CumulativeParameters<Var> {
    pub fn new(tasks: Vec<Task<Var>>, capacity: i32, horizon: i32) -> CumulativeParameters<Var> {
        CumulativeParameters {
            tasks: tasks
                .into_iter()
                .map(Rc::new)
                .collect::<Vec<_>>()
                .into_boxed_slice(),
            capacity,
            bounds: Vec::new(),
            updated: Vec::new(),
            horizon,
        }
    }
}

/// Stores the explanations
pub struct Explanation<Var> {
    /// The domain change related to the event; contains the type of domain change and the value
    pub change: ChangeWithBound,
    /// The updated task
    pub task: Rc<Task<Var>>,
    /// The actual explanation consisting of a PropositionalConjunction
    pub explanation: PropositionalConjunction,
}

impl<Var: IntVar + 'static> Explanation<Var> {
    pub fn new(
        change: ChangeWithBound,
        task: Rc<Task<Var>>,
        explanation: PropositionalConjunction,
    ) -> Explanation<Var> {
        Explanation {
            change,
            task,
            explanation,
        }
    }
}
