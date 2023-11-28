use crate::{
    basic_types::{
        variables::IntVar, Inconsistency, PredicateConstructor, PropagationStatusCP,
        PropositionalConjunction,
    },
    engine::{
        DomainChange, EmptyDomain, EnqueueDecision, PropagationContext, PropagatorVariable,
    },
};

use super::{Task, Updated, Util};

pub struct PropagationEvent<Var: IntVar + 'static> {
    pub lower_bound: bool,
    pub propagating_task: Task<Var>,
    pub update_value: i32,
    pub profile_tasks: Vec<Task<Var>>,
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
        tasks: &[Task<Var>],
        bounds: &mut Vec<(i32, i32)>,
        capacity: i32,
    ) -> (
        PropagationStatusCP,
        Vec<(bool, usize, i32, PropositionalConjunction)>,
    );

    /// Propagates from scratch (i.e. it recalculates all data structures)
    fn propagate_from_scratch(
        &mut self,
        context: &mut PropagationContext,
        tasks: &[Task<Var>],
        bounds: &mut Vec<(i32, i32)>,
        horizon: i32,
        capacity: i32,
    ) -> (
        PropagationStatusCP,
        Vec<(bool, usize, i32, PropositionalConjunction)>,
    );

    /// Checks whether the propagator should propagate based on the changes in task
    fn should_propagate(
        &mut self,
        context: &PropagationContext,
        _tasks: &[Task<Var>],
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
        tasks: &[Task<Var>],
        horizon: i32,
        capacity: i32,
    );

    /// Create the error clause consisting of the lower- and upper-bounds of the provided conflict tasks
    fn create_error_clause(
        &self,
        context: &PropagationContext,
        tasks: &[Task<Var>],
        conflict_tasks: &Vec<usize>,
    ) -> PropagationStatusCP {
        let mut error_clause = Vec::with_capacity(conflict_tasks.len() * 2);
        for task_id in conflict_tasks.iter() {
            let Task {
                start_variable: s,
                processing_time: _,
                resource_usage: _,
                id: _,
            } = &tasks[*task_id];
            error_clause.push(s.upper_bound_predicate(context.upper_bound(s)));
            error_clause.push(s.lower_bound_predicate(context.lower_bound(s)));
        }

        Err(Inconsistency::from(PropositionalConjunction::from(
            error_clause,
        )))
    }

    /// Eagerly store the reason for a propagation of a value in the appropriate datastructure
    fn store_explanation(
        &mut self,
        var: &PropagatorVariable<Var>,
        value: i32,
        explanation: PropositionalConjunction,
        lower_bound: bool,
    );

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
        (var, value): (&PropagatorVariable<Var>, i32),
        tasks: &[Task<Var>],
        profile_tasks: &[usize],
    ) -> (bool, PropositionalConjunction) {
        let explanation = Util::create_naïve_explanation(
            change_and_explanation_bound,
            var,
            context,
            profile_tasks.iter().map(|index| &tasks[*index]),
        );
        let result = match change_and_explanation_bound {
            DomainChange::LowerBound(_) => context.set_lower_bound(var, value),
            DomainChange::UpperBound(_) => context.set_upper_bound(var, value),
            _ => unreachable!(),
        };
        match result {
            Result::Err(_x) => (true, explanation),
            Result::Ok(_x) => (false, explanation),
        }
    }

    /// Propagate from scratch (i.e. recalculate all data structures)
    fn debug_propagate_from_scratch(
        &self,
        context: &mut PropagationContext,
        horizon: i32,
        capacity: i32,
        tasks_arg: &[Task<Var>],
        bounds: &[(i32, i32)],
    ) -> PropagationStatusCP;
}
