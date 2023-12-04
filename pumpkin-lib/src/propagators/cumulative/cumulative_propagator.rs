use crate::{
    basic_types::{
        variables::IntVar, Inconsistency, PropagationStatusCP, PropositionalConjunction,
    },
    engine::{
        CPPropagatorConstructor, ConstraintProgrammingPropagator, Delta, DomainEvent,
        EnqueueDecision, LocalId, OpaqueDomainEvent, PropagationContext,
        PropagatorConstructorContext, PropagatorVariable,
    },
    pumpkin_assert_eq_simple, pumpkin_assert_extreme,
};

use std::{hash::Hash, rc::Rc};

use super::{CumulativePropagationResult, IncrementalPropagator, TimeTablePerPoint};

#[derive(Clone, Copy, Debug, PartialEq)]
///Determines whether the propagator is incremental or not
pub enum Incrementality {
    REGULAR = 1,
    INCREMENTAL = 2,
}
#[derive(Clone, Debug, Copy)]
///Determines what method is used for propagation
pub enum PropagationMethod {
    TimeTablePerPoint = 1,
}

impl std::fmt::Display for PropagationMethod {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PropagationMethod::TimeTablePerPoint => write!(f, "TimeTablePerPoint"),
        }
    }
}

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
    pub horizon: i32,
    pub incrementality: Incrementality,
    pub propagation_method: PropagationMethod,
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
pub struct Cumulative<Var> {
    tasks: Box<[Rc<Task<Var>>]>,
    capacity: i32,
    horizon: i32,
    incrementality: Incrementality,
    _propagation_method: PropagationMethod,
    bounds: Vec<(i32, i32)>,
    updated: Vec<Updated<Var>>,
    propagator: Box<dyn IncrementalPropagator<Var>>,
}

impl<Var: IntVar + 'static + std::fmt::Debug> Cumulative<Var> {
    fn new(
        tasks: Vec<Task<Var>>,
        capacity: i32,
        horizon: i32,
        incrementality: Incrementality,
        propagation_method: PropagationMethod,
    ) -> Cumulative<Var> {
        let num_tasks = tasks.len();
        Cumulative {
            tasks: tasks
                .into_iter()
                .map(Rc::new)
                .collect::<Vec<_>>()
                .into_boxed_slice(),
            capacity,
            horizon,
            incrementality,
            _propagation_method: propagation_method,
            bounds: Vec::with_capacity(num_tasks),
            updated: Vec::new(),
            propagator: match propagation_method {
                PropagationMethod::TimeTablePerPoint => Box::new(TimeTablePerPoint::new(num_tasks)),
            },
        }
    }
}

impl<Var> CPPropagatorConstructor for Cumulative<Var>
where
    Var: IntVar + 'static + std::fmt::Debug,
{
    type Args = CumulativeArgs<Var>;

    fn create(
        args: Self::Args,
        mut context: PropagatorConstructorContext<'_>,
    ) -> Box<dyn ConstraintProgrammingPropagator> {
        //We order the tasks by non-decreasing (increasing in case of unique resource usage) resource usage, this allows certain optimizations
        let mut ordered_tasks = args.tasks.clone();
        ordered_tasks.sort_by(|a, b| b.resource_usage.cmp(&a.resource_usage));

        let mut id = 0;
        let tasks = ordered_tasks
            .iter()
            .filter_map(|x| {
                //We only add tasks which have a non-zero resource usage
                if x.resource_usage > 0 {
                    let return_value = Some(Task {
                        start_variable: context.register(
                            x.start_time.clone(),
                            DomainEvent::Any,
                            LocalId::from(id),
                        ), //Subscribe to all domain events concerning the current variable
                        processing_time: x.processing_time,
                        resource_usage: x.resource_usage,
                        id: LocalId::from(id),
                    });
                    id += 1;
                    return_value
                } else {
                    None
                }
            })
            .collect::<Vec<Task<Var>>>();
        Box::new(Cumulative::new(
            tasks,
            args.capacity,
            args.horizon,
            args.incrementality,
            args.propagation_method,
        ))
    }
}

impl<Var: IntVar + 'static> Cumulative<Var> {
    ///Propagates using the [propagator][Cumulative::propagator] and based on the value of `incremental`
    fn propagate_and_store_explanations(
        &mut self,
        context: &mut PropagationContext,
        incrementality: Incrementality,
    ) -> PropagationStatusCP {
        let CumulativePropagationResult {
            status,
            explanations,
        } = if let Incrementality::INCREMENTAL = incrementality {
            //Propagate incrementally, this requires the propagator to implement the propagate_incrementally
            self.propagator.propagate_incrementally(
                context,
                &mut self.updated,
                &self.tasks,
                self.capacity,
            )
        } else {
            //Propagates from scratch by recalculating all of the associated data structures from scratch
            self.propagator.propagate_from_scratch(
                context,
                &self.tasks,
                self.horizon,
                self.capacity,
            )
        };
        //We go over all of the explanations and store them
        if let Some(explanations) = explanations {
            for explanation in explanations {
                self.propagator.store_explanation(explanation)
            }
        }

        self.updated.clear(); //All of the updates (should) have been processed so we can clear the structure
        status
    }
}

impl<Var: IntVar + 'static> ConstraintProgrammingPropagator for Cumulative<Var> {
    fn propagate(&mut self, context: &mut PropagationContext) -> PropagationStatusCP {
        pumpkin_assert_extreme!(
            self.incrementality == Incrementality::REGULAR
                || self
                    .tasks
                    .iter()
                    .all(|current| self.bounds[current.id.get_value()]
                        == (
                            context.lower_bound(&current.start_variable),
                            context.upper_bound(&current.start_variable)
                        )),
            "It should be the case that the current known bounds are equal to the bounds of the context but got the following differences: {:?}",
            self.tasks
                .iter()
                .map(|current| format!(
                    "{:?} - {:?}",
                    self.bounds[current.id.get_value()],
                    (
                        context.lower_bound(&current.start_variable),
                        context.upper_bound(&current.start_variable)
                    )
                ))
                .collect::<Vec<_>>()
        );
        self.propagate_and_store_explanations(context, self.incrementality)
    }

    fn notify(
        &mut self,
        context: &mut PropagationContext,
        local_id: LocalId,
        _event: OpaqueDomainEvent,
    ) -> EnqueueDecision {
        pumpkin_assert_extreme!(
            self.incrementality != Incrementality::INCREMENTAL || {
                let task = &self.tasks[local_id.get_value()];
                context.lower_bound(&task.start_variable) >= self.bounds[task.id.get_value()].0
                    || self.bounds[task.id.get_value()].1
                        <= context.upper_bound(&task.start_variable)
            }, "The task has been updated but the provided bound is more general than expected, this could be due to improper resetting of the bounds during synchronisation"
        );
        match self.incrementality {
            Incrementality::INCREMENTAL => {
                let task = &self.tasks[local_id.get_value()];
                pumpkin_assert_eq_simple!(local_id.get_value(), task.id.get_value(), "The propagator works under the assumption that the `local_id` is the index into `self.tasks`");
                let (lower_bound, upper_bound) = self.bounds[task.id.get_value()];
                if context.lower_bound(&task.start_variable) > lower_bound
                    || context.upper_bound(&task.start_variable) < upper_bound
                {
                    //Lets the propagator decide whether it should propagate, this allows some optimizations based on, for example, mandatory parts
                    let decision = self.propagator.should_propagate(
                        context,
                        &self.tasks,
                        task,
                        &self.bounds,
                        self.capacity,
                        &mut self.updated,
                    );
                    //Set the known bounds to the current values of the context
                    let (lower_bound, upper_bound) =
                        self.bounds.get_mut(task.id.get_value()).unwrap();
                    *lower_bound = context.lower_bound(&task.start_variable);
                    *upper_bound = context.upper_bound(&task.start_variable);
                    return decision;
                }
                EnqueueDecision::Enqueue
            }
            Incrementality::REGULAR => EnqueueDecision::Enqueue,
        }
    }

    fn synchronise(&mut self, context: &PropagationContext) {
        //Backtrack, it could be the case that the current updates are invalid so clear thestructure
        self.updated.clear();
        if self.incrementality == Incrementality::INCREMENTAL {
            //The incremental methods make use of the bounds so reset the bounds to the current level
            self.bounds.clear();
            for task in self.tasks.iter() {
                self.bounds.push((
                    context.lower_bound(&task.start_variable),
                    context.upper_bound(&task.start_variable),
                ))
            }
        }
        if self.incrementality == Incrementality::INCREMENTAL {
            //Reset the structures on the propagator itself; generally this only occurs when the propagator is incremental since structures are recalculated from scratch otherwise
            self.propagator
                .reset_structures(context, &self.tasks, self.capacity);
        }
    }

    fn get_reason_for_propagation(
        &mut self,
        _context: &PropagationContext,
        delta: Delta,
    ) -> PropositionalConjunction {
        let affected_task = &self.tasks[delta.affected_local_id().get_value()];
        pumpkin_assert_eq_simple!(
            affected_task.id.get_value(),
            delta.affected_local_id().get_value()
        );
        self.propagator
            .get_reason(affected_task, affected_task.start_variable.unpack(delta))
    }

    fn priority(&self) -> u32 {
        1
    }

    fn name(&self) -> &str {
        "Cumulative"
    }

    fn initialise_at_root(&mut self, context: &mut PropagationContext) -> PropagationStatusCP {
        for task in self.tasks.iter() {
            if task.resource_usage > self.capacity {
                //There is a task which overflows the resource capacity on its own, this leads to a root-level conflict
                return Err(Inconsistency::Other(PropositionalConjunction::from(vec![])));
            }
        }
        if self.incrementality == Incrementality::INCREMENTAL {
            //If the propagation method is incremental then reset the bounds
            self.bounds.clear();
            for task in self.tasks.iter() {
                self.bounds.push((
                    context.lower_bound(&task.start_variable),
                    context.upper_bound(&task.start_variable),
                ))
            }
        }

        self.updated.clear();
        self.propagator.initialise(context, self.capacity)?;
        self.propagate_and_store_explanations(context, Incrementality::REGULAR)
    }

    fn debug_propagate_from_scratch(
        &self,
        context: &mut PropagationContext,
    ) -> PropagationStatusCP {
        self.propagator.debug_propagate_from_scratch(
            context,
            self.horizon,
            self.capacity,
            &self.tasks,
        )
    }
}
