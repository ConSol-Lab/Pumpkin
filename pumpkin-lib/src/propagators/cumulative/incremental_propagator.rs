use std::rc::Rc;

use crate::{
    basic_types::{variables::IntVar, PropagationStatusCP, PropositionalConjunction},
    engine::{DomainChange, EnqueueDecision, PropagationContext},
};

use super::{Task, Updated};

///Stores the explanations
/// * `change` - The domain change related to the event; contains the type of domain change and the value
/// * `index` - The index of the updated task (this is equal to its local id)
/// * `explanation` - The actual explanation consisting of a PropositionalConjunction
pub struct Explanation<Var> {
    pub change: DomainChange,
    pub task: Rc<Task<Var>>,
    pub explanation: PropositionalConjunction,
}

impl<Var: IntVar + 'static> Explanation<Var> {
    pub fn new(
        change: DomainChange,
        task: Rc<Task<Var>>,
        explanation: PropositionalConjunction,
    ) -> Explanation<Var> {
        Explanation {
            change,
            task,
            explanation,
        }
    }
}

///Stores the result of a propagation iteration by the cumulative propagators
/// * `status` - The result of the propagation, determining whether there was a conflict or whether it was
/// * `explanations` - The explanations found during the propagation cycle; these explanations are required to be added to the appropriate structures before. These explanations could be [None] if a structural inconsistency is found
pub struct CumulativePropagationResult<Var> {
    pub status: PropagationStatusCP,
    pub explanations: Option<Vec<Explanation<Var>>>,
}

impl<Var: IntVar + 'static> CumulativePropagationResult<Var> {
    pub fn new(
        status: PropagationStatusCP,
        explanations: Option<Vec<Explanation<Var>>>,
    ) -> CumulativePropagationResult<Var> {
        CumulativePropagationResult {
            status,
            explanations,
        }
    }
}

///The interface/trait which specifies the required behaviour of an incremental propagator
pub trait IncrementalPropagator<Var: IntVar + 'static> {
    ///Initialises the propagator constructs which could not be initialised with the ConstructorContext
    fn initialise(&mut self, _context: &PropagationContext, _capacity: i32) -> PropagationStatusCP {
        Ok(())
    }

    /// Returns the reason for why the affected task had the provided change
    fn get_reason(
        &self,
        affected_task: &Rc<Task<Var>>,
        change: DomainChange,
    ) -> PropositionalConjunction;

    /// Propagates incrementally (i.e. it only updates certain structures)
    fn propagate_incrementally(
        &mut self,
        context: &mut PropagationContext,
        updated: &mut Vec<Updated<Var>>,
        tasks: &[Rc<Task<Var>>],
        capacity: i32,
    ) -> CumulativePropagationResult<Var>;

    /// Propagates from scratch (i.e. it recalculates all data structures)
    fn propagate_from_scratch(
        &mut self,
        context: &mut PropagationContext,
        tasks: &[Rc<Task<Var>>],
        horizon: i32,
        capacity: i32,
    ) -> CumulativePropagationResult<Var>;

    /// Checks whether the propagator should propagate based on the changes in task, the default implementation adds the updated task and enqueues the propagator
    ///
    /// This method should likely be overriden by the propagator
    fn should_propagate(
        &mut self,
        context: &PropagationContext,
        _tasks: &[Rc<Task<Var>>],
        updated_task: &Rc<Task<Var>>,
        bounds: &[(i32, i32)],
        _capacity: i32,
        updated: &mut Vec<Updated<Var>>,
    ) -> EnqueueDecision {
        //Checks whether there is a change in the bounds
        let (prev_lower_bound, prev_upper_bound) = bounds[updated_task.id.get_value()];
        updated.push(Updated {
            task: Rc::clone(updated_task),
            old_lower_bound: prev_lower_bound,
            old_upper_bound: prev_upper_bound,
            new_lower_bound: context.lower_bound(&updated_task.start_variable),
            new_upper_bound: context.upper_bound(&updated_task.start_variable),
        });
        EnqueueDecision::Enqueue
    }

    /// Resets the data structures after backtracking/backjumping
    fn reset_structures(
        &mut self,
        context: &PropagationContext,
        tasks: &[Rc<Task<Var>>],
        capacity: i32,
    );

    /// Eagerly store the reason for a propagation of a value in the appropriate datastructure
    fn store_explanation(&mut self, explanation: Explanation<Var>);

    /// Propagate from scratch (i.e. recalculate all data structures)
    fn debug_propagate_from_scratch(
        &self,
        context: &mut PropagationContext,
        horizon: i32,
        capacity: i32,
        tasks_arg: &[Rc<Task<Var>>],
    ) -> PropagationStatusCP;
}
