#![cfg(test)]
//! This module exposes helpers that aid testing of CP propagators. The [`TestSolver`] allows
//! setting up specific scenarios under which to test the various operations of a propagator.
use crate::basic_types::{
    DomainId, Inconsistency, Literal, PropagationStatusCP, PropositionalConjunction,
    PropositionalVariable,
};
use crate::engine::{
    AssignmentsInteger, AssignmentsPropositional, CPPropagatorConstructor,
    ConstraintProgrammingPropagator, Delta, DomainChange, EmptyDomain, EnqueueDecision,
    IntDomainEvent, LocalId, OpaqueDomainEvent, PropagationContext, PropagatorConstructorContext,
    PropagatorId, WatchListCP,
};
use std::fmt::{Debug, Formatter};

use super::{DomainEvents, WatchListPropositional};

/// A container for CP variables, which can be used to test propagators.
#[derive(Default)]
pub struct TestSolver {
    assignment: AssignmentsInteger,
    assignment_propositional: AssignmentsPropositional,
    watch_list: WatchListCP,
    watch_list_propositional: WatchListPropositional,
    next_id: u32,
}

/// A wrapper around a propagator which also keeps track of the ID of the propagator in the test solver.
pub struct TestPropagator {
    propagator: Box<dyn ConstraintProgrammingPropagator>,
    id: PropagatorId,
}

impl Debug for TestPropagator {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "TestPropagator for {} (id: {})",
            self.propagator.name(),
            self.id
        )
    }
}

impl TestSolver {
    pub fn new_variable(&mut self, lb: i32, ub: i32) -> DomainId {
        self.watch_list.grow();
        self.assignment.grow(lb, ub)
    }

    pub fn new_literal(&mut self) -> Literal {
        let new_variable_index = self.assignment_propositional.num_propositional_variables();
        self.watch_list_propositional.grow();
        self.assignment_propositional.grow();

        Literal::new(PropositionalVariable::new(new_variable_index), true)
    }

    pub fn new_propagator<Constructor: CPPropagatorConstructor>(
        &mut self,
        args: Constructor::Args,
    ) -> Result<TestPropagator, Inconsistency> {
        let id = PropagatorId(self.next_id);
        self.next_id += 1;

        let propagator = Constructor::create(
            args,
            PropagatorConstructorContext::new(
                &mut self.watch_list,
                &mut self.watch_list_propositional,
                id,
            ),
        );

        let mut propagator1 = TestPropagator { propagator, id };
        self.initialise_at_root(&mut propagator1)?;

        Ok(propagator1)
    }

    pub fn initialise_at_root(&mut self, propagator: &mut TestPropagator) -> PropagationStatusCP {
        propagator
            .propagator
            .initialise_at_root(&mut PropagationContext::new(
                &mut self.assignment,
                &mut self.assignment_propositional,
                propagator.id,
            ))
    }

    pub fn contains(&self, var: DomainId, value: i32) -> bool {
        self.assignment.is_value_in_domain(var, value)
    }

    pub fn lower_bound(&self, var: DomainId) -> i32 {
        self.assignment.get_lower_bound(var)
    }

    pub fn increase_lower_bound(
        &mut self,
        propagator: &mut TestPropagator,
        id: i32,
        var: DomainId,
        value: i32,
    ) -> EnqueueDecision {
        let _ = self.assignment.tighten_lower_bound(var, value, None);
        let mut context = PropagationContext::new(
            &mut self.assignment,
            &mut self.assignment_propositional,
            propagator.id,
        );
        propagator.propagator.notify(
            &mut context,
            LocalId::from(id as u32),
            crate::engine::OpaqueDomainEvent::from(DomainEvents::LOWER_BOUND),
        )
    }

    pub fn upper_bound(&self, var: DomainId) -> i32 {
        self.assignment.get_upper_bound(var)
    }
    pub fn set_lower_bound(&mut self, var: DomainId, bound: i32) -> Result<(), EmptyDomain> {
        self.assignment.tighten_lower_bound(var, bound, None)
    }

    pub fn set_upper_bound(&mut self, var: DomainId, bound: i32) -> Result<(), EmptyDomain> {
        self.assignment.tighten_upper_bound(var, bound, None)
    }

    pub fn set_literal(&mut self, var: Literal, val: bool) {
        self.assignment_propositional
            .enqueue_decision_literal(if val { var } else { !var });
    }

    pub fn is_literal_true(&mut self, var: Literal) -> bool {
        self.assignment_propositional.is_literal_assigned_true(var)
    }

    pub fn is_literal_false(&mut self, var: Literal) -> bool {
        self.assignment_propositional.is_literal_assigned_false(var)
    }

    pub fn remove(&mut self, var: DomainId, value: i32) -> Result<(), EmptyDomain> {
        self.assignment.remove_value_from_domain(var, value, None)
    }

    pub fn propagate(&mut self, propagator: &mut TestPropagator) -> PropagationStatusCP {
        let mut context = PropagationContext::new(
            &mut self.assignment,
            &mut self.assignment_propositional,
            propagator.id,
        );
        propagator.propagator.propagate(&mut context)
    }

    pub fn notify(
        &mut self,
        propagator: &mut TestPropagator,
        event: OpaqueDomainEvent,
        local_id: LocalId,
    ) -> EnqueueDecision {
        propagator.propagator.notify(
            &mut PropagationContext::new(
                &mut self.assignment,
                &mut self.assignment_propositional,
                propagator.id,
            ),
            local_id,
            event,
        )
    }

    pub fn get_affected_locals(
        &self,
        propagator: &TestPropagator,
        domain_id: DomainId,
        event: IntDomainEvent,
    ) -> impl Iterator<Item = LocalId> + '_ {
        let id = propagator.id;
        self.watch_list
            .get_affected_propagators(event, domain_id)
            .iter()
            .filter_map(move |pvi| {
                if pvi.propagator == id {
                    Some(pvi.variable)
                } else {
                    None
                }
            })
    }

    pub fn notify_changed(
        &mut self,
        propagator: &mut TestPropagator,
        domain_id: DomainId,
        event: IntDomainEvent,
    ) {
        for local_id in self
            .get_affected_locals(propagator, domain_id, event)
            .collect::<Vec<_>>()
        {
            self.notify(propagator, event.into(), local_id);
        }
    }

    pub fn get_reason(
        &mut self,
        propagator: &mut TestPropagator,
        delta: Delta,
    ) -> PropositionalConjunction {
        let context = PropagationContext::new(
            &mut self.assignment,
            &mut self.assignment_propositional,
            propagator.id,
        );
        propagator
            .propagator
            .get_reason_for_propagation(&context, delta)
    }

    pub fn to_deltas(
        &self,
        propagator: &TestPropagator,
        var: DomainId,
        change: DomainChange,
    ) -> Vec<Delta> {
        self.watch_list
            .get_affected_propagators(change.into(), var)
            .iter()
            .filter(|pv| pv.propagator == propagator.id)
            .map(|pv| Delta::new(pv.variable, change))
            .collect()
    }

    pub fn assert_bounds(&self, var: DomainId, lb: i32, ub: i32) {
        let actual_lb = self.lower_bound(var);
        let actual_ub = self.upper_bound(var);

        assert_eq!((lb, ub), (actual_lb, actual_ub), "The expected bounds [{lb}..{ub}] did not match the actual bounds [{actual_lb}..{actual_ub}]");
    }
}
