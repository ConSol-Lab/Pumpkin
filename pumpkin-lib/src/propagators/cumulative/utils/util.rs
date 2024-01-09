use std::rc::Rc;

use crate::engine::{EmptyDomain, ReadDomains};
use crate::{
    basic_types::{
        variables::IntVar, Inconsistency, Predicate, PredicateConstructor, PropagationStatusCP,
        PropositionalConjunction,
    },
    engine::{
        DomainEvents, LocalId, PropagationContext, PropagationContextMut,
        PropagatorConstructorContext,
    },
    pumpkin_assert_simple,
};

use super::{ArgTask, CumulativeParameters, Task, Updated};

pub enum ChangeWithBound {
    LowerBound(i32),
    UpperBound(i32),
}

pub struct Util {}

impl Util {
    /// Creates an explanation consisting of all bounds of the variables causing a propagation (See "Improving scheduling by Learning (Section 4.5) - Andreas Schutt")
    ///
    /// `change_and_explanation_bound` stores the change (i.e. lower-bound or upper-bound change)
    /// and the explanation bound which should be used
    /// (Note that the explanation bound could be different from the actual propagation)
    pub fn create_naïve_explanation<'a, Var: IntVar + 'static>(
        change_and_explanation_bound: &ChangeWithBound,
        task: &Rc<Task<Var>>,
        context: &PropagationContextMut,
        profile_tasks: impl Iterator<Item = &'a Rc<Task<Var>>>,
    ) -> PropositionalConjunction {
        let mut explanation: Vec<Predicate> = Vec::new();

        //First we include the lower- or upper-bound of the task
        match change_and_explanation_bound {
            ChangeWithBound::LowerBound(explanation_bound) => {
                explanation.push(
                    task.start_variable
                        .lower_bound_predicate(*explanation_bound),
                );
            }
            ChangeWithBound::UpperBound(explanation_bound) => explanation.push(
                task.start_variable
                    .upper_bound_predicate(*explanation_bound),
            ),
        }

        //Then we go through all of the tasks and add their lower/upper-bounds to the explanation
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

    /// Create the error clause consisting of the lower- and upper-bounds of the provided conflict tasks
    pub fn create_error_clause<Var: IntVar + 'static>(
        context: &PropagationContextMut,
        conflict_tasks: &[Rc<Task<Var>>],
    ) -> PropagationStatusCP {
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

        Err(Inconsistency::from(PropositionalConjunction::from(
            error_clause,
        )))
    }

    /// Propagates the start variable of `propagating_task` to the provided `propagation_value` and eagerly calculates the explanation given the `profile_tasks` which were responsible for the propagation
    pub fn propagate_and_explain<Var: IntVar + 'static>(
        context: &mut PropagationContextMut,
        change_and_explanation_bound: ChangeWithBound,
        propagating_task: &Rc<Task<Var>>,
        propagation_value: i32,
        profile_tasks: &[Rc<Task<Var>>],
    ) -> Result<(), EmptyDomain> {
        pumpkin_assert_simple!(
            !profile_tasks.is_empty(),
            "A propagation has to have occurred due to another task"
        );
        let explanation = Util::create_naïve_explanation(
            &change_and_explanation_bound,
            propagating_task,
            context,
            profile_tasks.iter(),
        );
        match change_and_explanation_bound {
            ChangeWithBound::LowerBound(_) => context.set_lower_bound(
                &propagating_task.start_variable,
                propagation_value,
                explanation,
            ),
            ChangeWithBound::UpperBound(_) => context.set_upper_bound(
                &propagating_task.start_variable,
                propagation_value,
                explanation,
            ),
        }
    }

    /// Based on the [ArgTask]s which are passed, it creates and returns [Task]s which have been registered for bound events and the horizon
    ///
    /// This method ensures that the tasks are ordered in non-decreasing order by resource usage and that tasks with a resource usage of zero are removed
    pub fn create_tasks<Var: IntVar + 'static>(
        arg_tasks: &[ArgTask<Var>],
        mut context: PropagatorConstructorContext<'_>,
    ) -> (Vec<Task<Var>>, i32) {
        //We order the tasks by non-decreasing (increasing in case of unique resource usage) resource usage, this allows certain optimizations
        let mut ordered_tasks = arg_tasks.to_vec();
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
                            DomainEvents::BOUNDS,
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
        (
            tasks,
            ordered_tasks
                .iter()
                .map(|current| current.processing_time)
                .sum::<i32>(),
        )
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

    /// Updates the bounds of the provided `task` to those stored in `context`
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

    /// Clears the provided `updated` and resets **all** bounds to those stored in `context`
    ///
    /// This method is predominantly used during bactracking/synchronisation
    pub fn reset_bounds_clear_updated<Var: IntVar + 'static>(
        context: &PropagationContext,
        updated: &mut Vec<Updated<Var>>,
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
}
