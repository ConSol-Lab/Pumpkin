use std::rc::Rc;

use crate::{
    basic_types::{
        variables::IntVar, Inconsistency, PredicateConstructor, PropagationStatusCP,
        PropositionalConjunction,
    },
    engine::{DomainChange, EmptyDomain, EnqueueDecision, PropagationContext, PropagatorVariable},
};

use super::{Task, Updated, Util};

///Stores the explanations
/// * `change` - The domain change related to the event; contains the type of domain change and the value
/// * `index` - The index of the updated task (this is equal to its local id)
/// * `explanation` - The actual explanation consisting of a PropositionalConjunction
pub struct Explanation {
    pub change: DomainChange,
    pub index: usize,
    pub explanation: PropositionalConjunction,
}

impl Explanation {
    pub fn new(
        change: DomainChange,
        index: usize,
        explanation: PropositionalConjunction,
    ) -> Explanation {
        Explanation {
            change,
            index,
            explanation,
        }
    }
}

///Stores the result of a propagation iteration by the cumulative propagators
/// * `status` - The result of the propagation, determining whether there was a conflict or whether it was
/// * `explanations` - The explanations found during the propagation cycle; these explanations are required to be added to the appropriate structures before. These explanations could be [None] if a structural inconsistency is found
pub struct CumulativePropagationResult {
    pub status: PropagationStatusCP,
    pub explanations: Option<Vec<Explanation>>,
}

impl CumulativePropagationResult {
    pub fn new(
        status: PropagationStatusCP,
        explanations: Option<Vec<Explanation>>,
    ) -> CumulativePropagationResult {
        CumulativePropagationResult {
            status,
            explanations,
        }
    }
}

pub trait IncrementalPropagator<Var: IntVar + 'static> {
    ///Initialises the propagator constructs which could not be initialised with the ConstructorContext
    fn initialise(&mut self, _context: &PropagationContext, _capacity: i32) -> PropagationStatusCP {
        Ok(())
    }

    /// Returns the reason for why the affected task had the provided change
    fn get_reason(
        &self,
        affected_task: &Task<Var>,
        change: DomainChange,
    ) -> PropositionalConjunction;

    /// Propagates incrementally (i.e. it only updates certain structures)
    fn propagate_incrementally(
        &mut self,
        context: &mut PropagationContext,
        updated: &mut Vec<Updated>,
        tasks: &[Rc<Task<Var>>],
        capacity: i32,
    ) -> CumulativePropagationResult;

    /// Propagates from scratch (i.e. it recalculates all data structures)
    fn propagate_from_scratch(
        &mut self,
        context: &mut PropagationContext,
        tasks: &[Rc<Task<Var>>],
        horizon: i32,
        capacity: i32,
    ) -> CumulativePropagationResult;

    /// Checks whether the propagator should propagate based on the changes in task
    fn should_propagate(
        &mut self,
        context: &PropagationContext,
        _tasks: &[Rc<Task<Var>>],
        task: &Task<Var>,
        bounds: &[(i32, i32)],
        _capacity: i32,
        updated: &mut Vec<Updated>,
    ) -> EnqueueDecision {
        //Checks whether there is a change in the bounds
        let (lower_bound, upper_bound) = bounds[task.id.get_value()];
        if lower_bound < context.lower_bound(&task.start_variable)
            || upper_bound > context.upper_bound(&task.start_variable)
        {
            updated.push(Updated {
                task_id: task.id.get_value(),
                old_lower_bound: lower_bound,
                old_upper_bound: upper_bound,
                new_lower_bound: context.lower_bound(&task.start_variable),
                new_upper_bound: context.upper_bound(&task.start_variable),
            })
        }
        if !updated.is_empty() {
            crate::engine::EnqueueDecision::Enqueue
        } else {
            EnqueueDecision::Skip
        }
    }

    /// Resets the data structures after backtracking/backjumping
    fn reset_structures(
        &mut self,
        context: &PropagationContext,
        tasks: &[Rc<Task<Var>>],
        capacity: i32,
    );

    /// Create the error clause consisting of the lower- and upper-bounds of the provided conflict tasks
    fn create_error_clause(
        &self,
        context: &PropagationContext,
        conflict_tasks: &Vec<Rc<Task<Var>>>,
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

    /// Eagerly store the reason for a propagation of a value in the appropriate datastructure
    fn store_explanation(&mut self, explanation: Explanation);

    /// Propagate the variable to the provided value
    fn propagate_without_explanation(
        &self,
        context: &mut PropagationContext,
        lower_bound: bool,
        (var, value): (&PropagatorVariable<Var>, i32),
    ) -> Result<(), EmptyDomain> {
        if lower_bound {
            context.set_lower_bound(var, value)
        } else {
            context.set_upper_bound(var, value)
        }
    }

    /// Propagate the variable to the provided value and eagerly calculate the explanation given the tasks which were responsible for the propagation
    fn propagate_and_explain(
        &self,
        context: &mut PropagationContext,
        change_and_explanation_bound: DomainChange,
        propagating_variable: &PropagatorVariable<Var>,
        propagation_value: i32,
        profile_tasks: &[Rc<Task<Var>>],
    ) -> Result<PropositionalConjunction, PropositionalConjunction> {
        let explanation = Util::create_naïve_explanation(
            change_and_explanation_bound,
            propagating_variable,
            context,
            profile_tasks.iter(),
        );
        let result = match change_and_explanation_bound {
            DomainChange::LowerBound(_) => {
                context.set_lower_bound(propagating_variable, propagation_value)
            }
            DomainChange::UpperBound(_) => {
                context.set_upper_bound(propagating_variable, propagation_value)
            }
            _ => unreachable!(),
        };
        match result {
            Result::Err(_) => Err(explanation),
            Result::Ok(_) => Ok(explanation),
        }
    }

    /// Propagate from scratch (i.e. recalculate all data structures)
    fn debug_propagate_from_scratch(
        &self,
        context: &mut PropagationContext,
        horizon: i32,
        capacity: i32,
        tasks_arg: &[Rc<Task<Var>>],
    ) -> PropagationStatusCP;
}
