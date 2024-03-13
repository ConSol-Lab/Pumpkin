//! Contains common methods for all of the cumulative propagators found in
//! [`crate::propagators::cumulative`]; this includes methods for propagating
//! ([`propagate_and_explain`]) but also methods related to creating the input parameters
//! ([`create_tasks`]).
use std::rc::Rc;

use crate::basic_types::variables::IntVar;
use crate::basic_types::Inconsistency;
use crate::basic_types::PredicateConstructor;
use crate::basic_types::PropositionalConjunction;
use crate::engine::cp::propagation::ReadDomains;
use crate::engine::domain_events::DomainEvents;
use crate::engine::propagation::local_id::LocalId;
use crate::engine::propagation::propagation_context::PropagationContext;
use crate::engine::propagation::propagation_context::PropagationContextMut;
use crate::engine::propagation::propagator_constructor_context::PropagatorConstructorContext;
use crate::engine::EmptyDomain;
use crate::propagators::ArgTask;
use crate::propagators::CumulativeParameters;
use crate::propagators::Task;
use crate::propagators::UpdatedTaskInfo;
use crate::pumpkin_assert_simple;

/// An enum containing possible domain changes (lower-bound and upper-bound) with the bound used for
/// explaining the change. For example, let's say we propagate [x >= 5] and this is due to the
/// lower-bound [x >= 2] then the [`ChangeWithExplanationBound`] will be `LowerBound(2)`.
#[derive(Debug, Copy, Clone)]
pub enum ChangeWithExplanationBound {
    LowerBound(i32),
    UpperBound(i32),
}

/// Creates an explanation consisting of all bounds of the variables causing a propagation (See [Section 4.5 of \[1\]](http://cp2013.a4cp.org/sites/default/files/andreas_schutt_-_improving_scheduling_by_learning.pdf))
///
/// `change_and_explanation_bound` stores the change (i.e. lower-bound or upper-bound change)
/// and the explanation bound which should be used
///
/// \[1\] A. Schutt, Improving scheduling by learning. University of Melbourne, Department of
/// Computer Science and Software Engineering, 2011.
pub fn create_naive_explanation<'a, Var: IntVar + 'static>(
    change_and_explanation_bound: &ChangeWithExplanationBound,
    task: &Rc<Task<Var>>,
    context: &PropagationContextMut,
    profile_tasks: impl Iterator<Item = &'a Rc<Task<Var>>>,
) -> PropositionalConjunction {
    let mut explanation = vec![
        // First we include the lower- or upper-bound of the task
        match change_and_explanation_bound {
            ChangeWithExplanationBound::LowerBound(explanation_bound) => task
                .start_variable
                .lower_bound_predicate(*explanation_bound),
            ChangeWithExplanationBound::UpperBound(explanation_bound) => task
                .start_variable
                .upper_bound_predicate(*explanation_bound),
        },
    ];

    // Then we go through all of the tasks and add their lower/upper-bounds to the explanation
    for task in profile_tasks {
        explanation.push(
            task.start_variable
                .lower_bound_predicate(context.lower_bound(&task.start_variable)),
        );
        explanation.push(
            task.start_variable
                .upper_bound_predicate(context.upper_bound(&task.start_variable)),
        );
    }
    PropositionalConjunction::from(explanation)
}

/// Create the [`Inconsistency`] consisting of the lower- and upper-bounds of the provided conflict
/// [`Task`]s
pub fn create_inconsistency<Var: IntVar + 'static>(
    context: &PropagationContextMut,
    conflict_tasks: &[Rc<Task<Var>>],
) -> Inconsistency {
    let mut error_clause = Vec::with_capacity(conflict_tasks.len() * 2);
    for task in conflict_tasks.iter() {
        error_clause.push(
            task.start_variable
                .upper_bound_predicate(context.upper_bound(&task.start_variable)),
        );
        error_clause.push(
            task.start_variable
                .lower_bound_predicate(context.lower_bound(&task.start_variable)),
        );
    }

    Inconsistency::from(PropositionalConjunction::from(error_clause))
}

/// Propagates the start variable of [`propagating_task`][Task] to the provided `propagation_value`
/// and eagerly calculates the [`explanation`][PropositionalConjunction] given the `profile_tasks`
/// which were responsible for the propagation
pub fn propagate_and_explain<Var: IntVar + 'static>(
    context: &mut PropagationContextMut,
    change_and_explanation_bound: ChangeWithExplanationBound,
    propagating_task: &Rc<Task<Var>>,
    propagation_value: i32,
    profile_tasks: &[Rc<Task<Var>>],
) -> Result<(), EmptyDomain> {
    pumpkin_assert_simple!(
        !profile_tasks.is_empty(),
        "A propagation has to have occurred due to another task"
    );
    let explanation = create_naive_explanation(
        &change_and_explanation_bound,
        propagating_task,
        context,
        profile_tasks.iter(),
    );
    match change_and_explanation_bound {
        ChangeWithExplanationBound::LowerBound(_) => context.set_lower_bound(
            &propagating_task.start_variable,
            propagation_value,
            explanation,
        ),
        ChangeWithExplanationBound::UpperBound(_) => context.set_upper_bound(
            &propagating_task.start_variable,
            propagation_value,
            explanation,
        ),
    }
}

/// Based on the [`ArgTask`]s which are passed, it creates and returns [`Task`]s which have been
/// registered for [`DomainEvents`]
///
/// It sorts [`Task`]s on non-decreasing resource usage and removes [`Task`]s with resource usage 0.
pub fn create_tasks<Var: IntVar + 'static>(
    arg_tasks: &[ArgTask<Var>],
    mut context: PropagatorConstructorContext<'_>,
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

/// Initialises the bounds at the root
pub fn initialise_at_root<Var: IntVar + 'static>(
    update_bounds: bool,
    params: &mut CumulativeParameters<Var>,
    context: &PropagationContextMut,
) {
    if update_bounds {
        params.bounds.clear();
        for task in params.tasks.iter() {
            params.bounds.push((
                context.lower_bound(&task.start_variable),
                context.upper_bound(&task.start_variable),
            ))
        }
    }

    params.updated.clear();
}

/// Updates the bounds of the provided [`Task`] to those stored in
/// [`context`][PropagationContextMut]
pub fn update_bounds_task<Var: IntVar + 'static>(
    context: &PropagationContextMut,
    bounds: &mut [(i32, i32)],
    task: &Rc<Task<Var>>,
) {
    bounds[task.id.unpack() as usize] = (
        context.lower_bound(&task.start_variable),
        context.upper_bound(&task.start_variable),
    );
}

/// Clears the provided `updated` and resets **all** bounds to those stored in
/// [`context`][PropagationContext]
///
/// This method is currently used during bactracking/synchronisation
pub fn reset_bounds_clear_updated<Var: IntVar + 'static>(
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
pub fn check_bounds_equal_at_propagation<Var: IntVar + 'static>(
    context: &mut PropagationContextMut,
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
