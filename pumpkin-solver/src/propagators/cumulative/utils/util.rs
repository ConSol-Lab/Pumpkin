//! Contains common methods for all of the propagators of the cumulative constraint; this includes
//! methods for propagating but also methods related to creating the
//! input parameters.
use std::rc::Rc;

use enumset::enum_set;

use crate::containers::KeyedVec;
use crate::engine::cp::propagation::ReadDomains;
use crate::engine::domain_events::DomainEvents;
use crate::engine::propagation::local_id::LocalId;
use crate::engine::propagation::PropagationContext;
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
) -> (Vec<Task<Var>>, KeyedVec<LocalId, usize>) {
    // We order the tasks by non-decreasing resource usage, this allows certain optimizations
    let mut ordered_tasks = arg_tasks
        .iter()
        .enumerate()
        .map(|(index, arg_task)| (arg_task, index))
        .collect::<Vec<_>>();
    ordered_tasks.sort_by(|(a, _), (b, _)| b.resource_usage.cmp(&a.resource_usage));

    let mut mapping: KeyedVec<LocalId, usize> = KeyedVec::new();

    let mut id = 0;
    let tasks = ordered_tasks
        .iter()
        .filter_map(|(x, index)| {
            // We only add tasks which have a non-zero resource usage
            if x.resource_usage > 0 {
                let return_value = Some(Task {
                    start_variable: x.start_time.clone(),
                    processing_time: x.processing_time,
                    resource_usage: x.resource_usage,
                    id: LocalId::from(id),
                });
                let _ = mapping.push(*index);

                id += 1;
                return_value
            } else {
                None
            }
        })
        .collect::<Vec<Task<Var>>>();
    (tasks, mapping)
}

pub(crate) fn register_tasks<Var: IntegerVariable + 'static>(
    tasks: &[Rc<Task<Var>>],
    context: &mut PropagatorInitialisationContext<'_>,
    register_backtrack: bool,
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
                DomainEvents::create_with_int_events(enum_set!(
                    IntDomainEvent::LowerBound
                        | IntDomainEvent::UpperBound
                        | IntDomainEvent::Assign
                )),
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
