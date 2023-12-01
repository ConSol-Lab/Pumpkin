use std::rc::Rc;

use crate::{
    basic_types::{
        variables::IntVar, Inconsistency, Predicate, PredicateConstructor, PropagationStatusCP,
        PropositionalConjunction,
    },
    engine::{DomainChange, PropagationContext},
    pumpkin_assert_simple,
};

use super::Task;

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
}
