use crate::{
    basic_types::{variables::IntVar, PropagationStatusCP, PropositionalConjunction},
    engine::{DomainChange, LocalId, PropagatorVariable},
};
use std::hash::Hash;
use std::rc::Rc;

#[derive(Debug)]
///Structure which stores the variables related to a task; for now, only the start times are assumed to be variable
/// * `start_variable` - The [PropagatorVariable] representing the start time of a task
/// * `processing_time` - The processing time of the `start_variable` (also referred to as duration of a task)
/// * `resource_usage` - How much of the resource the given task uses during its non-preemptive execution
/// * `id` - The [LocalId] of the task, this corresponds with its index into [tasks][Cumulative::tasks]
pub struct Task<Var> {
    pub start_variable: PropagatorVariable<Var>,
    pub processing_time: i32,
    pub resource_usage: i32,
    pub id: LocalId,
}

impl<Var: IntVar + 'static> Hash for Task<Var> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

impl<Var: IntVar + 'static> PartialEq for Task<Var> {
    fn eq(&self, other: &Self) -> bool {
        self.id.get_value() == other.id.get_value()
    }
}

impl<Var: IntVar + 'static> Eq for Task<Var> {}

#[derive(Clone, Debug)]
///The task which is passed as argument
/// * `start_variable` - The [IntVar] representing the start time of a task
/// * `processing_time` - The processing time of the `start_variable` (also referred to as duration of a task)
/// * `resource_usage` - How much of the resource the given task uses during its non-preemptive execution
pub struct ArgTask<Var> {
    pub start_time: Var,
    pub processing_time: i32,
    pub resource_usage: i32,
}
#[derive(Clone)]
///The arguments which are required to create the constraint/propagators
/// * `tasks` - A box containing all of the ArgTasks
/// * `capacity` - The capacity of the resource
/// * `horizon` - The horizon of the resource (i.e. the largest possible makespan)
/// * `incrementality` - What form of incrementality is used (either [Incrementality::INCREMENTAL] or [Incrementality::REGULAR])
/// * `propagation_method` - The used propagation method (for example, [PropagationMethod::TimeTablePerPoint])
pub struct CumulativeArgs<Var> {
    pub tasks: Box<[ArgTask<Var>]>,
    pub capacity: i32,
}

#[derive(Debug)]
/// Stores the information of an updated task
pub struct Updated<Var> {
    pub task: Rc<Task<Var>>,
    pub old_lower_bound: i32,
    pub old_upper_bound: i32,
    pub new_lower_bound: i32,
    pub new_upper_bound: i32,
}

///Holds the data for the cumulative constraint; this constraint models that for each time-point in the horizon, the resource consumption for that time-point is not exceeded
/// * `tasks` - The Set of Tasks; for each task, the [LocalId] is assumed to correspond to its index in this [Vec]; this is stored as a Box of Rc's to accomodate the sharing of the tasks
/// * `capacity` - The capacity of the resource (i.e. how much resource consumption can be maximally accomodated at each time point)
/// * `horizon` - The horizon of the resource (i.e. the largest possible makespan)
/// * `incrementality` - What form of incrementality is used (either [Incrementality::INCREMENTAL] or [Incrementality::REGULAR])
/// * `propagation_method` - The used propagation method (for example, [PropagationMethod::TimeTablePerPoint])
/// * `bounds` - The current known bounds of the different tasks; stored as (lower_bound, upper_bound)
/// * `updated` - The variables which have been updated since the last round of propagation, this structure is updated by the (incremental) propagator
/// * `propagator` - Holds the actual propagator which is responsible for the propagation, this allows the overarching propagator to not be required to have unnecessary data structures
pub struct CumulativeParameters<Var> {
    pub tasks: Box<[Rc<Task<Var>>]>,
    pub capacity: i32,
    pub bounds: Vec<(i32, i32)>,
    pub updated: Vec<Updated<Var>>,
}

impl<Var: IntVar + 'static> CumulativeParameters<Var> {
    pub fn create(tasks: Vec<Task<Var>>, capacity: i32) -> CumulativeParameters<Var> {
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

///Stores the explanations
/// * `change` - The domain change related to the event; contains the type of domain change and the value
/// * `index` - The index of the updated task (this is equal to its local id)
/// * `explanation` - The actual explanation consisting of a PropositionalConjunction
pub struct Explanation<Var> {
    pub change: DomainChange,
    pub task: Rc<Task<Var>>,
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

///Stores the result of a propagation iteration by the cumulative propagators
/// * `status` - The result of the propagation, determining whether there was a conflict or whether it was
/// * `explanations` - The explanations found during the propagation cycle; these explanations are required to be added to the appropriate structures before. These explanations could be [None] if a structural inconsistency is found
pub struct CumulativePropagationResult<Var> {
    pub status: PropagationStatusCP,
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
