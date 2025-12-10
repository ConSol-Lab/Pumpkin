use std::rc::Rc;

use super::Task;
use crate::engine::cp::propagation::contexts::propagation_context::ReadDomains;
use crate::engine::propagation::PropagationContext;
use crate::propagators::CumulativePropagatorOptions;
use crate::variables::IntegerVariable;

/// Holds the data for the cumulative constraint; more specifically it holds:
/// - The tasks
/// - The capacity of the resource
/// - The options for propagating the cumulative constraint
#[derive(Debug, Clone)]
pub(crate) struct CumulativeParameters<Var, PVar, RVar, CVar> {
    /// The Set of [`Task`]s; for each [`Task`], the [`Task::id`] is assumed to correspond to its
    /// index in this [`Vec`]; this is stored as a [`Box`] of [`Rc`]'s to accomodate the
    /// sharing of the tasks
    pub(crate) tasks: Box<[Rc<Task<Var, PVar, RVar>>]>,
    /// The capacity of the resource (i.e. how much resource consumption can be maximally
    /// accomodated at each time point)
    pub(crate) capacity: CVar,
    /// The [`CumulativeOptions`] which influence the behaviour of the cumulative propagator(s).
    pub(crate) options: CumulativePropagatorOptions,
    /// Indicates that the constraint is infeasible.
    ///
    /// This can occur when there is a task with a resource usage higher than the capacity.
    pub(crate) is_infeasible: bool,
}

impl<
    Var: IntegerVariable + 'static,
    PVar: IntegerVariable + 'static,
    RVar: IntegerVariable + 'static,
    CVar: IntegerVariable + 'static,
> CumulativeParameters<Var, PVar, RVar, CVar>
{
    pub(crate) fn new(
        context: PropagationContext,
        tasks: Vec<Task<Var, PVar, RVar>>,
        capacity: CVar,
        options: CumulativePropagatorOptions,
    ) -> CumulativeParameters<Var, PVar, RVar, CVar> {
        let mut is_infeasible = false;
        let tasks = tasks
            .into_iter()
            .map(|task| {
                is_infeasible |=
                    context.lower_bound(&task.resource_usage) > context.upper_bound(&capacity);
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
