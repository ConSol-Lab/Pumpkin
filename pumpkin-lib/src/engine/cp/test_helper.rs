//! This module exposes helpers that aid testing of CP propagators. The [`TestSolver`] allows
//! setting up specific scenarios under which to test the various operations of a propagator.
use crate::basic_types::{DomainId, PropagationStatusCP, PropositionalConjunction};
use crate::engine::EmptyDomain;

use super::{EnqueueDecision, LocalId};
use super::{
    propagation::{
        CPPropagatorConstructor, ConstraintProgrammingPropagator, Delta, PropagationContext,
        PropagatorConstructorContext, PropagatorId,
    },
    AssignmentsInteger, WatchListCP,
};

/// A container for CP variables, which can be used to test propagators.
#[derive(Default)]
pub struct TestSolver {
    assignment: AssignmentsInteger,
    watch_list: WatchListCP,
    next_id: u32,
}

/// A wrapper around a propagator which also keeps track of the ID of the propagator in the test solver.
pub struct TestPropagator {
    propagator: Box<dyn ConstraintProgrammingPropagator>,
    id: PropagatorId,
}

impl TestSolver {
    pub fn new_variable(&mut self, lb: i32, ub: i32) -> DomainId {
        self.watch_list.grow();
        self.assignment.grow(lb, ub)
    }

    pub fn initialise_at_root(&mut self, propagator: &mut TestPropagator) -> PropagationStatusCP {
        let mut context = PropagationContext::new(&mut self.assignment, propagator.id);
        propagator.propagator.initialise_at_root(&mut context)
    }

    pub fn new_propagator<Constructor: CPPropagatorConstructor>(
        &mut self,
        args: Constructor::Args,
    ) -> TestPropagator {
        let id = PropagatorId(self.next_id);
        self.next_id += 1;

        let propagator = Constructor::create(
            args,
            PropagatorConstructorContext::new(&mut self.watch_list, id),
        );

        TestPropagator { propagator, id }
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
        let mut context = PropagationContext::new(&mut self.assignment, propagator.id);
        propagator.propagator.notify(
            &mut context,
            LocalId::from(id as u32),
            crate::engine::OpaqueDomainEvent::from(crate::engine::DomainEvent::LowerBound),
        )
    }

    pub fn upper_bound(&self, var: DomainId) -> i32 {
        self.assignment.get_upper_bound(var)
    }

    pub fn remove(&mut self, var: DomainId, value: i32) -> Result<(), EmptyDomain> {
        self.assignment.remove_value_from_domain(var, value, None)
    }

    pub fn propagate(&mut self, propagator: &mut TestPropagator) -> PropagationStatusCP {
        let mut context = PropagationContext::new(&mut self.assignment, propagator.id);
        propagator.propagator.propagate(&mut context)
    }

    pub fn get_reason(
        &mut self,
        propagator: &mut TestPropagator,
        delta: Delta,
    ) -> PropositionalConjunction {
        let context = PropagationContext::new(&mut self.assignment, propagator.id);
        propagator
            .propagator
            .get_reason_for_propagation(&context, delta)
    }

    pub fn assert_bounds(&self, var: DomainId, lb: i32, ub: i32) {
        let actual_lb = self.lower_bound(var);
        let actual_ub = self.upper_bound(var);

        assert_eq!((lb, ub), (actual_lb, actual_ub), "The expected bounds [{lb}..{ub}] did not match the actual bounds [{actual_lb}..{actual_ub}]");
    }
}
