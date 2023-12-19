use crate::propagators::TimeTablePerPointIncrementalProp;
use crate::{
    basic_types::{variables::IntVar, PropagationStatusCP, PropositionalConjunction},
    engine::{DomainChange, LocalId, PropagatorVariable},
    propagators::TimeTablePerPointProp,
};
use std::rc::Rc;
use std::{hash::Hash, marker::PhantomData};

#[derive(Debug)]
/// Structure which stores the variables related to a task; for now, only the start times are assumed to be variable
pub struct Task<Var> {
    /// * `start_variable` - The [PropagatorVariable] representing the start time of a task
    pub start_variable: PropagatorVariable<Var>,
    /// * `processing_time` - The processing time of the `start_variable` (also referred to as duration of a task)
    pub processing_time: i32,
    /// * `resource_usage` - How much of the resource the given task uses during its non-preemptive execution
    pub resource_usage: i32,
    /// * `id` - The [LocalId] of the task, this corresponds with its index into [tasks][Cumulative::tasks]
    pub id: LocalId,
}

impl<Var: IntVar + 'static> Task<Var> {
    pub fn get_id(task: &Rc<Task<Var>>) -> usize {
        task.id.unpack()
    }
}

impl<Var: IntVar + 'static> Hash for Task<Var> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

impl<Var: IntVar + 'static> PartialEq for Task<Var> {
    fn eq(&self, other: &Self) -> bool {
        self.id.unpack::<usize>() == other.id.unpack()
    }
}

impl<Var: IntVar + 'static> Eq for Task<Var> {}

#[derive(Clone, Debug)]
/// The task which is passed as argument
pub struct ArgTask<Var> {
    /// * `start_variable` - The [IntVar] representing the start time of a task
    pub start_time: Var,
    /// * `processing_time` - The processing time of the `start_variable` (also referred to as duration of a task)
    pub processing_time: i32,
    /// * `resource_usage` - How much of the resource the given task uses during its non-preemptive execution
    pub resource_usage: i32,
}
#[derive(Clone)]
/// The arguments which are required to create the constraint/propagators
pub struct CumulativeArgs<Var, T> {
    /// * `tasks` - A box containing all of the ArgTasks
    pub tasks: Box<[ArgTask<Var>]>,
    /// * `capacity` - The capacity of the resource
    pub capacity: i32,
    /// * `propagator_type` - We use [PhantomData] to differentiate between the different types of propagators, without this field we would need to create a new argument struct for each cumulative propagator
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

/// An alias used for calling the [CumulativeArgs::new] method with the concrete propagator type of [TimeTablePerPointProp]; this is used to prevent creating a different `new` method for each type `T`
pub type TimeTablePerPoint<Var> = CumulativeArgs<Var, TimeTablePerPointProp<Var>>;
pub type TimeTablePerPointIncremental<Var> =
    CumulativeArgs<Var, TimeTablePerPointIncrementalProp<Var>>;

#[derive(Debug)]
/// Stores the information of an updated task
pub struct Updated<Var> {
    pub task: Rc<Task<Var>>,
    pub old_lower_bound: i32,
    pub old_upper_bound: i32,
    pub new_lower_bound: i32,
    pub new_upper_bound: i32,
}

/// Holds the data for the cumulative constraint; this constraint models that for each time-point in the horizon, the resource consumption for that time-point is not exceeded
pub struct CumulativeParameters<Var> {
    /// * `tasks` - The Set of Tasks; for each task, the [LocalId] is assumed to correspond to its index in this [Vec]; this is stored as a Box of Rc's to accomodate the sharing of the tasks
    pub tasks: Box<[Rc<Task<Var>>]>,
    /// * `capacity` - The capacity of the resource (i.e. how much resource consumption can be maximally accomodated at each time point)
    pub capacity: i32,
    /// * `bounds` - The current known bounds of the different tasks; stored as (lower_bound, upper_bound)
    pub bounds: Vec<(i32, i32)>,
    /// * `updated` - The variables which have been updated since the last round of propagation, this structure is updated by the (incremental) propagator
    pub updated: Vec<Updated<Var>>,
    /// * `horizon` - The largest possible makespan, in this case it is assumed to be the sum of all processing times
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
    /// * `change` - The domain change related to the event; contains the type of domain change and the value
    pub change: DomainChange,
    /// * `task` - The updated task
    pub task: Rc<Task<Var>>,
    /// * `explanation` - The actual explanation consisting of a PropositionalConjunction
    pub explanation: PropositionalConjunction,
}

impl<Var: IntVar + 'static> Explanation<Var> {
    pub fn new(
        change: DomainChange,
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

/// Stores the result of a propagation iteration by the cumulative propagators
pub struct CumulativePropagationResult<Var> {
    /// * `status` - The result of the propagation, determining whether there was a conflict or whether it was
    pub status: PropagationStatusCP,
    /// * `explanations` - The explanations found during the propagation cycle; these explanations are required to be added to the appropriate structures before. These explanations could be [None] if a structural inconsistency is found
    pub explanations: Option<Vec<Explanation<Var>>>,
}

impl<Var: IntVar + 'static> CumulativePropagationResult<Var> {
    pub fn new(
        status: PropagationStatusCP,
        explanations: Option<Vec<Explanation<Var>>>,
    ) -> CumulativePropagationResult<Var> {
        CumulativePropagationResult {
            status,
            explanations,
        }
    }
}
