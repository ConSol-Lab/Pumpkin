#![cfg(any(test, doc))]
//! This module exposes helpers that aid testing of CP propagators. The [`TestSolver`] allows
//! setting up specific scenarios under which to test the various operations of a propagator.
use std::fmt::Debug;
use std::fmt::Formatter;

use super::propagation::EnqueueDecision;
use crate::basic_types::Inconsistency;
use crate::basic_types::PropositionalConjunction;
use crate::engine::opaque_domain_event::OpaqueDomainEvent;
use crate::engine::predicates::predicate::Predicate;
use crate::engine::propagation::LocalId;
use crate::engine::propagation::PropagationContext;
use crate::engine::propagation::PropagationContextMut;
use crate::engine::propagation::Propagator;
use crate::engine::propagation::PropagatorConstructor;
use crate::engine::propagation::PropagatorConstructorContext;
use crate::engine::propagation::PropagatorId;
use crate::engine::reason::ReasonStore;
use crate::engine::variables::DomainId;
use crate::engine::variables::IntegerVariable;
use crate::engine::variables::Literal;
use crate::engine::Assignments;
use crate::engine::DomainEvents;
use crate::engine::EmptyDomain;
use crate::engine::IntDomainEvent;
use crate::engine::WatchListCP;
use crate::predicate;

/// A container for CP variables, which can be used to test propagators.
#[derive(Default, Debug)]
pub struct TestSolver {
    assignments: Assignments,
    reason_store: ReasonStore,
    watch_list: WatchListCP,
    next_id: u32,
}

type BoxedPropagator = Box<dyn Propagator>;

impl Debug for BoxedPropagator {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "test_helper::Propagator(<boxed value>)")
    }
}

impl TestSolver {
    pub fn new_variable(&mut self, lb: i32, ub: i32) -> DomainId {
        self.watch_list.grow();
        self.assignments.grow(lb, ub)
    }

    pub fn new_literal(&mut self) -> Literal {
        let domain_id = self.new_variable(0, 1);
        Literal::new(predicate!(domain_id == 1))
    }

    pub fn new_propagator<Constructor>(
        &mut self,
        constructor: Constructor,
    ) -> Result<BoxedPropagator, Inconsistency>
    where
        Constructor: PropagatorConstructor,
        Constructor::Propagator: 'static,
    {
        let id = PropagatorId(self.next_id);
        self.next_id += 1;

        let mut propagator =
            constructor.create_boxed(PropagatorConstructorContext::new(&mut self.watch_list, id));

        self.initialise_at_root(&mut propagator)?;

        Ok(propagator)
    }

    pub fn get_propagation_context_mut(
        &mut self,
        propagator_id: PropagatorId,
    ) -> PropagationContextMut {
        PropagationContextMut {
            assignments: &mut self.assignments,
            reason_store: &mut self.reason_store,
            propagator_id,
        }
    }

    pub fn initialise_at_root(
        &mut self,
        propagator: &mut BoxedPropagator,
    ) -> Result<(), Inconsistency> {
        propagator.initialise_at_root(&mut PropagationContextMut::new(
            &mut self.assignments,
            &mut self.reason_store,
            PropagatorId(0),
        ))
    }

    pub fn contains<Var: IntegerVariable>(&self, var: Var, value: i32) -> bool {
        var.contains(&self.assignments, value)
    }

    pub fn lower_bound(&self, var: DomainId) -> i32 {
        self.assignments.get_lower_bound(var)
    }

    pub fn increase_lower_bound_and_notify(
        &mut self,
        propagator: &mut BoxedPropagator,
        id: i32,
        var: DomainId,
        value: i32,
    ) -> EnqueueDecision {
        let result = self.assignments.tighten_lower_bound(var, value, None);
        assert!(result.is_ok(), "The provided value to `increase_lower_bound` caused an empty domain, generally the propagator should not be notified of this change!");
        let mut context = PropagationContextMut::new(
            &mut self.assignments,
            &mut self.reason_store,
            PropagatorId(0),
        );
        propagator.notify(
            &mut context,
            LocalId::from(id as u32),
            OpaqueDomainEvent::from(
                DomainEvents::LOWER_BOUND
                    .get_int_events()
                    .iter()
                    .next()
                    .unwrap(),
            ),
        )
    }

    pub fn is_literal_assigned(&self, literal: Literal) -> bool {
        self.assignments
            .evaluate_predicate(literal.into())
            .is_some()
    }

    pub fn is_literal_false(&self, literal: Literal) -> bool {
        self.assignments
            .evaluate_predicate(literal.into())
            .is_some_and(|truth_value| !truth_value)
    }

    pub fn set_lower_bound(&mut self, var: DomainId, bound: i32) -> Result<(), EmptyDomain> {
        self.assignments.tighten_lower_bound(var, bound, None)
    }

    pub fn set_upper_bound(&mut self, var: DomainId, bound: i32) -> Result<(), EmptyDomain> {
        self.assignments.tighten_upper_bound(var, bound, None)
    }

    pub fn upper_bound(&self, var: DomainId) -> i32 {
        self.assignments.get_upper_bound(var)
    }

    pub fn remove(&mut self, var: DomainId, value: i32) -> Result<(), EmptyDomain> {
        self.assignments.remove_value_from_domain(var, value, None)
    }

    pub fn set_literal(&mut self, literal: Literal, truth_value: bool) -> Result<(), EmptyDomain> {
        match truth_value {
            true => self.assignments.post_predicate(literal.into(), None),
            false => self.assignments.post_predicate((!literal).into(), None),
        }
    }

    pub fn propagate(&mut self, propagator: &mut BoxedPropagator) -> Result<(), Inconsistency> {
        let mut context = PropagationContextMut::new(
            &mut self.assignments,
            &mut self.reason_store,
            PropagatorId(0),
        );
        propagator.propagate(&mut context)
    }

    pub fn propagate_until_fixed_point(
        &mut self,
        propagator: &mut BoxedPropagator,
    ) -> Result<(), Inconsistency> {
        let mut num_trail_entries = self.assignments.num_trail_entries();
        self.notify_propagator(propagator);
        loop {
            {
                // Specify the life-times to be able to retrieve the trail entries
                let mut context = PropagationContextMut::new(
                    &mut self.assignments,
                    &mut self.reason_store,
                    PropagatorId(0),
                );
                propagator.propagate(&mut context)?;
                self.notify_propagator(propagator);
            }
            if self.assignments.num_trail_entries() == num_trail_entries {
                break;
            }
            num_trail_entries = self.assignments.num_trail_entries();
        }
        Ok(())
    }

    fn notify_propagator(&mut self, propagator: &mut BoxedPropagator) {
        let events = self.assignments.drain_domain_events().collect::<Vec<_>>();
        let mut context = PropagationContextMut::new(
            &mut self.assignments,
            &mut self.reason_store,
            PropagatorId(0),
        );
        for (event, domain) in events {
            for propagator_var in self.watch_list.get_affected_propagators(event, domain) {
                let _ = propagator.notify(&mut context, propagator_var.variable, event.into());
            }
        }
    }

    pub fn notify(
        &mut self,
        propagator: &mut BoxedPropagator,
        event: OpaqueDomainEvent,
        local_id: LocalId,
    ) -> EnqueueDecision {
        propagator.notify(
            &mut PropagationContextMut::new(
                &mut self.assignments,
                &mut self.reason_store,
                PropagatorId(0),
            ),
            local_id,
            event,
        )
    }

    pub fn notify_changed(
        &mut self,
        propagator: &mut BoxedPropagator,
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
            let _ = self.notify(propagator, opaque_event, pvi.variable);
        }
    }

    pub fn get_reason_int(&mut self, predicate: Predicate) -> &PropositionalConjunction {
        let reason_ref = self
            .assignments
            .get_reason_for_predicate_brute_force(predicate);
        let context = PropagationContext::new(&self.assignments);
        self.reason_store
            .get_or_compute(reason_ref, &context)
            .expect("reason_ref should not be stale")
    }

    pub fn get_reason_bool(
        &mut self,
        literal: Literal,
        truth_value: bool,
    ) -> &PropositionalConjunction {
        let predicate = match truth_value {
            true => literal.into(),
            false => (!literal).into(),
        };
        self.get_reason_int(predicate)
    }

    pub fn assert_bounds(&self, var: DomainId, lb: i32, ub: i32) {
        let actual_lb = self.lower_bound(var);
        let actual_ub = self.upper_bound(var);

        assert_eq!(
            (lb, ub), (actual_lb, actual_ub),
            "The expected bounds [{lb}..{ub}] did not match the actual bounds [{actual_lb}..{actual_ub}]"
        );
    }
}
