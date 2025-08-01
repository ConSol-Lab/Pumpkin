use std::rc::Rc;

use super::Task;
use crate::propagators::CumulativePropagatorOptions;
use crate::variables::IntegerVariable;

/// Holds the data for the cumulative constraint; more specifically it holds:
/// - The tasks
/// - The capacity of the resource
/// - The options for propagating the cumulative constraint
#[derive(Debug, Clone)]
pub(crate) struct CumulativeParameters<Var> {
    /// The Set of [`Task`]s; for each [`Task`], the [`Task::id`] is assumed to correspond to its
    /// index in this [`Vec`]; this is stored as a [`Box`] of [`Rc`]'s to accomodate the
    /// sharing of the tasks
    pub(crate) tasks: Box<[Rc<Task<Var>>]>,
    /// The capacity of the resource (i.e. how much resource consumption can be maximally
    /// accomodated at each time point)
    pub(crate) capacity: i32,
    /// The [`CumulativeOptions`] which influence the behaviour of the cumulative propagator(s).
    pub(crate) options: CumulativePropagatorOptions,
    /// Indicates that the constraint is infeasible.
    ///
    /// This can occur when there is a task with a resource usage higher than the capacity.
    pub(crate) is_infeasible: bool,
}

impl<Var: IntegerVariable + 'static> CumulativeParameters<Var> {
    pub(crate) fn new(
        tasks: Vec<Task<Var>>,
        capacity: i32,
        options: CumulativePropagatorOptions,
    ) -> CumulativeParameters<Var> {
        let mut is_infeasible = false;
        let tasks = tasks
            .into_iter()
            .map(|task| {
                is_infeasible |= task.resource_usage > capacity;
                Rc::new(task)
            })
            .collect::<Vec<_>>()
            .into_boxed_slice();

        CumulativeParameters {
            tasks: tasks.clone(),
            capacity,
            options,
            is_infeasible,
        }
    }
}
