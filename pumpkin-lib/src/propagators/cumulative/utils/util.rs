use std::{collections::HashMap, rc::Rc};

use crate::{
    basic_types::{
        variables::IntVar, Inconsistency, Predicate, PredicateConstructor, PropagationStatusCP,
        PropositionalConjunction,
    },
    engine::{
        Delta, DomainChange, DomainEvents, LocalId, PropagationContext,
        PropagatorConstructorContext,
    },
    pumpkin_assert_simple,
};

use super::{ArgTask, CumulativeParameters, Explanation, Task};

pub struct Util {}

impl Util {
    /// Creates an explanation consisting of all bounds of the variables causing a propagation (See "Improving scheduling by Learning (Section 4.5) - Andreas Schutt")
    /// * `change_and_explanation_bound` - The change (i.e. lower-bound or upper-bound change) and the explanation bound which should be used (Note that the explanation bound could be different from the actual propagation)
    pub fn create_naïve_explanation<'a, Var: IntVar + 'static>(
        change_and_explanation_bound: DomainChange,
        task: &Rc<Task<Var>>,
        context: &PropagationContext,
        profile_tasks: impl Iterator<Item = &'a Rc<Task<Var>>>,
    ) -> PropositionalConjunction {
        let mut explanation: Vec<Predicate> = Vec::new();

        //First we include the lower- or upper-bound of the task
        match change_and_explanation_bound {
            DomainChange::LowerBound(explanation_bound) => {
                explanation.push(task.start_variable.lower_bound_predicate(explanation_bound));
            }
            DomainChange::UpperBound(explanation_bound) => {
                explanation.push(task.start_variable.upper_bound_predicate(explanation_bound))
            }
            _ => unreachable!(),
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
        context: &PropagationContext,
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
        context: &mut PropagationContext,
        change_and_explanation_bound: DomainChange,
        propagating_task: &Rc<Task<Var>>,
        propagation_value: i32,
        profile_tasks: &[Rc<Task<Var>>],
    ) -> Result<PropositionalConjunction, PropositionalConjunction> {
        pumpkin_assert_simple!(
            !profile_tasks.is_empty(),
            "A propagation has to have occurred due to another task"
        );
        let explanation = Util::create_naïve_explanation(
            change_and_explanation_bound,
            propagating_task,
            context,
            profile_tasks.iter(),
        );
        let result = match change_and_explanation_bound {
            DomainChange::LowerBound(_) => {
                context.set_lower_bound(&propagating_task.start_variable, propagation_value)
            }
            DomainChange::UpperBound(_) => {
                context.set_upper_bound(&propagating_task.start_variable, propagation_value)
            }
            _ => unreachable!(),
        };
        match result {
            Result::Err(_) => Err(explanation),
            Result::Ok(_) => Ok(explanation),
        }
    }

    /// Based on the [ArgTask]s which are passed, it creates [Task]s which have been registered for bound events
    ///
    /// This method ensures that the tasks are ordered in non-decreasing order by resource usage and that tasks with a resource usage of zero are removed
    pub fn create_tasks<Var: IntVar + 'static>(
        arg_tasks: &[ArgTask<Var>],
        mut context: PropagatorConstructorContext<'_>,
    ) -> Vec<Task<Var>> {
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
        tasks
    }

    /// This task stores the propagations in the correct structure based on whether the explanation concerns a lower-bound update or an upper-bound update
    pub fn store_explanations<Var: IntVar + 'static>(
        explanations: Option<Vec<Explanation<Var>>>,
        reasons_for_propagation_lower_bound: &mut [HashMap<i32, PropositionalConjunction>],
        reasons_for_propagation_upper_bound: &mut [HashMap<i32, PropositionalConjunction>],
    ) {
        if let Some(explanations) = explanations {
            for Explanation {
                change,
                task,
                explanation,
            } in explanations
            {
                //Note that we assume that the index is the same as the local id of the task
                match change {
                    DomainChange::LowerBound(value) => {
                        reasons_for_propagation_lower_bound[task.id.unpack::<usize>()]
                            .insert(value, explanation);
                    }
                    DomainChange::UpperBound(value) => {
                        reasons_for_propagation_upper_bound[task.id.unpack::<usize>()]
                            .insert(value, explanation);
                    }
                    _ => unreachable!(),
                }
            }
        }
    }

    /// Returns the reason for a propagation based on the provided [Delta] and the structures in which the reasons are stored
    pub fn get_reason_for_propagation<Var: IntVar + 'static>(
        delta: Delta,
        reasons_for_propagation_lower_bound: &[HashMap<i32, PropositionalConjunction>],
        reasons_for_propagation_upper_bound: &[HashMap<i32, PropositionalConjunction>],
        tasks: &[Rc<Task<Var>>],
    ) -> PropositionalConjunction {
        let affected_task = &tasks[delta.affected_local_id().unpack::<usize>()];
        match affected_task.start_variable.unpack(delta) {
            DomainChange::LowerBound(value) => reasons_for_propagation_lower_bound
                [affected_task.id.unpack::<usize>()]
            .get(&value)
            .unwrap()
            .clone(),
            DomainChange::UpperBound(value) => reasons_for_propagation_upper_bound
                [affected_task.id.unpack::<usize>()]
            .get(&value)
            .unwrap()
            .clone(),
            _ => unreachable!(),
        }
    }

    /// Initialises the bounds at the root
    pub fn initialise_at_root<Var: IntVar + 'static>(
        update_bounds: bool,
        params: &mut CumulativeParameters<Var>,
        context: &PropagationContext,
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
}
