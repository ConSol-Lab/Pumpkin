//! Contains common methods for all of the propagators of the cumulative constraint; this includes
//! methods for propagating but also methods related to creating the
//! input parameters.
use std::rc::Rc;

use enumset::enum_set;

use crate::engine::cp::propagation::ReadDomains;
use crate::engine::domain_events::DomainEvents;
use crate::engine::propagation::local_id::LocalId;
use crate::engine::propagation::propagation_context::PropagationContext;
use crate::engine::propagation::PropagatorInitialisationContext;
use crate::engine::variables::IntegerVariable;
use crate::engine::IntDomainEvent;
use crate::propagators::ArgTask;
use crate::propagators::Task;

/// Based on the [`ArgTask`]s which are passed, it creates and returns [`Task`]s which have been
/// registered for [`DomainEvents`].
///
/// It sorts [`Task`]s on non-decreasing resource usage and removes [`Task`]s with resource usage 0.
pub(crate) fn create_tasks<Var: IntegerVariable + 'static>(
    arg_tasks: &[ArgTask<Var>],
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
                    start_variable: x.start_time.clone(),
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

pub(crate) fn register_tasks<Var: IntegerVariable + 'static>(
    tasks: &[Rc<Task<Var>>],
    context: &mut PropagatorInitialisationContext<'_>,
    register_backtrack: bool,
    allow_holes_in_domain: bool,
) {
    tasks.iter().for_each(|task| {
        let _ = context.register(
            task.start_variable.clone(),
            DomainEvents::create_with_int_events(enum_set!(
                IntDomainEvent::LowerBound | IntDomainEvent::UpperBound | IntDomainEvent::Assign
            )),
            task.id,
        );
        if register_backtrack {
            let _ = context.register_for_backtrack_events(
                task.start_variable.clone(),
                if allow_holes_in_domain {
                    // We differentiate between these two cases due to this example:
                    //
                    // Recall that for every non-updated profile, we only go through the tasks
                    // which have been updated.
                    //
                    // Now imagine a situation in which we are propagating holes in the domain but
                    // have not subscribed to the `Removal` events. Let's say we have a task `x`
                    // which has been updated at some trail entry `y`; we
                    // now propagate holes in the domain for this profile due to an updated profile
                    // `z` and then mark `x` and `z` as non-updated.
                    //
                    // Now we backtrack to a trail position `>y` and since we have not subscribed to
                    // `Removal` events, we are not notified that the holes in
                    // the domain of `x` have been undone. Now both `x` and `z`
                    // are marked as non-updated and `x` is thus not propagated due to `z` while we
                    // should have propagated `x` again since it was updated at
                    // the trail position to which we backtracked.
                    //
                    // This is the reason that we also subscribe to the backtrack Removal event when
                    // allowing holes in the domain and incrementally
                    // backtracking.
                    DomainEvents::create_with_int_events(enum_set!(
                        IntDomainEvent::LowerBound
                            | IntDomainEvent::UpperBound
                            | IntDomainEvent::Assign
                            | IntDomainEvent::Removal
                    ))
                } else {
                    DomainEvents::create_with_int_events(enum_set!(
                        IntDomainEvent::LowerBound
                            | IntDomainEvent::UpperBound
                            | IntDomainEvent::Assign
                    ))
                },
                task.id,
            );
        }
    });
}

/// Updates the bounds of the provided [`Task`] to those stored in
/// `context`.
pub(crate) fn update_bounds_task<Var: IntegerVariable + 'static>(
    context: PropagationContext,
    bounds: &mut [(i32, i32)],
    task: &Rc<Task<Var>>,
) {
    bounds[task.id.unpack() as usize] = (
        context.lower_bound(&task.start_variable),
        context.upper_bound(&task.start_variable),
    );
}

/// Determines whether the stored bounds are equal when propagation occurs
pub(crate) fn check_bounds_equal_at_propagation<Var: IntegerVariable + 'static>(
    context: PropagationContext,
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
