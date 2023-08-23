//! This module exposes helpers that aid testing of CP propagators. The [`TestSolver`] allows
//! setting up specific scenarios under which to test the various operations of a propagator.
use crate::basic_types::{DomainId, PropagationStatusCP, PropositionalConjunction};

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
}
