use std::rc::Rc;

use crate::{
    basic_types::{variables::IntVar, Predicate, PredicateConstructor, PropositionalConjunction},
    engine::{DomainChange, PropagationContext},
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
}
