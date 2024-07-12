//! Contains common methods for all of the propagators of the cumulative constraint; this includes
//! methods for propagating but also methods related to creating the
//! input parameters.
use std::rc::Rc;

use crate::engine::cp::propagation::ReadDomains;
use crate::engine::domain_events::DomainEvents;
use crate::engine::propagation::local_id::LocalId;
use crate::engine::propagation::propagation_context::PropagationContext;
use crate::engine::propagation::propagator_constructor_context::PropagatorConstructorContext;
use crate::engine::variables::IntegerVariable;
use crate::propagators::ArgTask;
use crate::propagators::Task;
use crate::propagators::UpdatedTaskInfo;

/// Based on the [`ArgTask`]s which are passed, it creates and returns [`Task`]s which have been
/// registered for [`DomainEvents`].
///
/// It sorts [`Task`]s on non-decreasing resource usage and removes [`Task`]s with resource usage 0.
pub(crate) fn create_tasks<Var: IntegerVariable + 'static>(
    arg_tasks: &[ArgTask<Var>],
    context: &mut PropagatorConstructorContext<'_>,
) -> Vec<Task<Var>> {
    // We order the tasks by non-decreasing resource usage, this allows certain optimizations
    let mut ordered_tasks = arg_tasks.to_vec();
    ordered_tasks.sort_by(|a, b| b.resource_usage.cmp(&a.resource_usage));

    let mut id = 0;
    ordered_tasks
        .iter()
        .filter_map(|x| {
            // We only add tasks which have a non-zero resource usage
            if x.resource_usage > 0 {
                let return_value = Some(Task {
                    start_variable: context.register(
                        x.start_time.clone(),
                        DomainEvents::BOUNDS,
                        LocalId::from(id),
                    ), // Subscribe to all domain events concerning the current variable
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
        .collect::<Vec<Task<Var>>>()
}

/// Updates the bounds of the provided [`Task`] to those stored in
/// `context`.
pub(crate) fn update_bounds_task<Var: IntegerVariable + 'static>(
    context: &PropagationContext,
    bounds: &mut [(i32, i32)],
    task: &Rc<Task<Var>>,
) {
    bounds[task.id.unpack() as usize] = (
        context.lower_bound(&task.start_variable),
        context.upper_bound(&task.start_variable),
    );
}

/// Clears the provided `updated` and resets **all** bounds to those stored in
/// `context`.
///
/// This method is currently used during bactracking/synchronisation
pub(crate) fn reset_bounds_clear_updated<Var: IntegerVariable + 'static>(
    context: &PropagationContext,
    updated: &mut Vec<UpdatedTaskInfo<Var>>,
    bounds: &mut Vec<(i32, i32)>,
    tasks: &[Rc<Task<Var>>],
) {
    updated.clear();
    bounds.clear();
    for task in tasks.iter() {
        bounds.push((
            context.lower_bound(&task.start_variable),
            context.upper_bound(&task.start_variable),
        ))
    }
}

/// Determines whether the stored bounds are equal when propagation occurs
pub(crate) fn check_bounds_equal_at_propagation<Var: IntegerVariable + 'static>(
    context: &PropagationContext,
    tasks: &[Rc<Task<Var>>],
    bounds: &[(i32, i32)],
) -> bool {
    tasks.iter().all(|current| {
        bounds[current.id.unpack() as usize]
            == (
                context.lower_bound(&current.start_variable),
                context.upper_bound(&current.start_variable),
            )
    })
}
