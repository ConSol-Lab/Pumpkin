#![cfg(test)]
//! This module exposes helpers that aid testing of CP propagators. The [`TestSolver`] allows
//! setting up specific scenarios under which to test the various operations of a propagator.
use crate::basic_types::{
    DomainId, Inconsistency, Literal, Predicate, PropagationStatusCP, PropositionalConjunction,
    PropositionalVariable,
};
use crate::engine::reason::ReasonStore;
use crate::engine::{
    AssignmentsInteger, AssignmentsPropositional, CPPropagatorConstructor,
    ConstraintProgrammingPropagator, EmptyDomain, EnqueueDecision, IntDomainEvent, LocalId,
    OpaqueDomainEvent, PropagationContext, PropagationContextMut, PropagatorConstructorContext,
    PropagatorId, WatchListCP,
};
use std::fmt::{Debug, Formatter};

use super::WatchListPropositional;

/// A container for CP variables, which can be used to test propagators.
#[derive(Default)]
pub struct TestSolver {
    assignments_integer: AssignmentsInteger,
    reason_store: ReasonStore,
    assignments_propositional: AssignmentsPropositional,
    watch_list: WatchListCP,
    watch_list_propositional: WatchListPropositional,
    next_id: u32,
}

type Propagator = Box<dyn ConstraintProgrammingPropagator>;

impl Debug for Propagator {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "test_helper::Propagator(<boxed value>)")
    }
}

impl TestSolver {
    pub fn new_variable(&mut self, lb: i32, ub: i32) -> DomainId {
        self.watch_list.grow();
        self.assignments_integer.grow(lb, ub)
    }

    pub fn new_literal(&mut self) -> Literal {
        let new_variable_index = self.assignments_propositional.num_propositional_variables();
        self.watch_list_propositional.grow();
        self.assignments_propositional.grow();

        Literal::new(PropositionalVariable::new(new_variable_index), true)
    }

    pub fn new_propagator<Constructor>(
        &mut self,
        constructor: Constructor,
    ) -> Result<Propagator, Inconsistency>
    where
        Constructor: CPPropagatorConstructor,
        Constructor::Propagator: 'static,
    {
        let id = PropagatorId(self.next_id);
        self.next_id += 1;

        let mut propagator = constructor.create_boxed(PropagatorConstructorContext::new(
            &mut self.watch_list,
            &mut self.watch_list_propositional,
            id,
        ));

        self.initialise_at_root(&mut propagator)?;

        Ok(propagator)
    }

    pub fn initialise_at_root(&mut self, propagator: &mut Propagator) -> PropagationStatusCP {
        propagator.initialise_at_root(&mut PropagationContextMut::new(
            &mut self.assignments_integer,
            &mut self.reason_store,
            &mut self.assignments_propositional,
        ))
    }

    pub fn contains(&self, var: DomainId, value: i32) -> bool {
        self.assignments_integer.is_value_in_domain(var, value)
    }

    pub fn lower_bound(&self, var: DomainId) -> i32 {
        self.assignments_integer.get_lower_bound(var)
    }

    pub fn set_lower_bound(&mut self, var: DomainId, bound: i32) -> Result<(), EmptyDomain> {
        self.assignments_integer
            .tighten_lower_bound(var, bound, None)
    }

    pub fn set_upper_bound(&mut self, var: DomainId, bound: i32) -> Result<(), EmptyDomain> {
        self.assignments_integer
            .tighten_upper_bound(var, bound, None)
    }

    pub fn set_literal(&mut self, var: Literal, val: bool) {
        self.assignments_propositional
            .enqueue_decision_literal(if val { var } else { !var });
    }

    pub fn is_literal_true(&self, var: Literal) -> bool {
        self.assignments_propositional.is_literal_assigned_true(var)
    }

    pub fn is_literal_false(&self, var: Literal) -> bool {
        self.assignments_propositional
            .is_literal_assigned_false(var)
    }

    pub fn upper_bound(&self, var: DomainId) -> i32 {
        self.assignments_integer.get_upper_bound(var)
    }

    pub fn remove(&mut self, var: DomainId, value: i32) -> Result<(), EmptyDomain> {
        self.assignments_integer
            .remove_value_from_domain(var, value, None)
    }

    pub fn propagate(&mut self, propagator: &mut Propagator) -> PropagationStatusCP {
        let mut context = PropagationContextMut::new(
            &mut self.assignments_integer,
            &mut self.reason_store,
            &mut self.assignments_propositional,
        );
        propagator.propagate(&mut context)
    }

    pub fn notify(
        &mut self,
        propagator: &mut Propagator,
        event: OpaqueDomainEvent,
        local_id: LocalId,
    ) -> EnqueueDecision {
        propagator.notify(
            &mut PropagationContextMut::new(
                &mut self.assignments_integer,
                &mut self.reason_store,
                &mut self.assignments_propositional,
            ),
            local_id,
            event,
        )
    }

    pub fn notify_changed(
        &mut self,
        propagator: &mut Propagator,
        id: DomainId,
        event: IntDomainEvent,
    ) {
        let opaque_event: OpaqueDomainEvent = event.into();
        let propagator_var_ids = self.watch_list.get_affected_propagators(event, id).to_vec();
        for pvi in propagator_var_ids {
            assert_eq!(
                pvi.propagator,
                PropagatorId(0),
                "We assume a single propagator per TestSolver in notify_changed"
            );
            let _ = self.notify(propagator, opaque_event.clone(), pvi.variable);
        }
    }

    pub fn get_reason_int(&mut self, predicate: Predicate) -> &PropositionalConjunction {
        let reason_ref = self.assignments_integer.get_reason_for_predicate(predicate);
        let context =
            PropagationContext::new(&self.assignments_integer, &self.assignments_propositional);
        self.reason_store
            .get_or_compute(reason_ref, &context)
            .expect("reason_ref should not be stale")
    }

    pub fn get_reason_bool(
        &mut self,
        literal: Literal,
        assignment: bool,
    ) -> &PropositionalConjunction {
        let reason_ref = self
            .assignments_propositional
            .get_reason_for_assignment(literal, assignment);
        let context =
            PropagationContext::new(&self.assignments_integer, &self.assignments_propositional);
        self.reason_store
            .get_or_compute(reason_ref, &context)
            .expect("reason_ref should not be stale")
    }

    pub fn assert_bounds(&self, var: DomainId, lb: i32, ub: i32) {
        let actual_lb = self.lower_bound(var);
        let actual_ub = self.upper_bound(var);

        assert_eq!((lb, ub), (actual_lb, actual_ub), "The expected bounds [{lb}..{ub}] did not match the actual bounds [{actual_lb}..{actual_ub}]");
    }
}
