#![cfg(any(test, doc))]
//! This module exposes helpers that aid testing of CP propagators. The [`TestSolver`] allows
//! setting up specific scenarios under which to test the various operations of a propagator.
use std::fmt::Debug;
use std::fmt::Formatter;

use super::propagation::store::PropagatorStore;
use super::propagation::EnqueueDecision;
use super::propagation::PropagatorInitialisationContext;
use crate::basic_types::Inconsistency;
use crate::basic_types::PropositionalConjunction;
use crate::engine::conflict_analysis::SemanticMinimiser;
use crate::engine::opaque_domain_event::OpaqueDomainEvent;
use crate::engine::predicates::predicate::Predicate;
use crate::engine::propagation::LocalId;
use crate::engine::propagation::PropagationContext;
use crate::engine::propagation::PropagationContextMut;
use crate::engine::propagation::Propagator;
use crate::engine::propagation::PropagatorId;
use crate::engine::reason::ReasonStore;
use crate::engine::variables::DomainId;
use crate::engine::variables::IntegerVariable;
use crate::engine::variables::Literal;
use crate::engine::Assignments;
use crate::engine::DomainEvents;
use crate::engine::EmptyDomain;
use crate::engine::WatchListCP;

/// A container for CP variables, which can be used to test propagators.
#[derive(Debug)]
pub(crate) struct TestSolver {
    assignments: Assignments,
    reason_store: ReasonStore,
    semantic_minimiser: SemanticMinimiser,
    watch_list: WatchListCP,
    next_propagator_id: u32,
}

impl Default for TestSolver {
    fn default() -> Self {
        let mut solver = Self {
            assignments: Default::default(),
            reason_store: Default::default(),
            semantic_minimiser: Default::default(),
            watch_list: Default::default(),
            next_propagator_id: Default::default(),
        };
        // We allocate space for the zero-th dummy variable at the root level of the assignments.
        solver.watch_list.grow();
        solver
    }
}

type BoxedPropagator = Box<dyn Propagator>;

impl Debug for BoxedPropagator {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "test_helper::Propagator(<boxed value>)")
    }
}

impl TestSolver {
    pub(crate) fn new_variable(&mut self, lb: i32, ub: i32) -> DomainId {
        self.watch_list.grow();
        self.assignments.grow(lb, ub)
    }

    pub(crate) fn new_literal(&mut self) -> Literal {
        let domain_id = self.new_variable(0, 1);
        Literal::new(domain_id)
    }

    pub(crate) fn new_propagator(
        &mut self,
        propagator: impl Propagator + 'static,
    ) -> Result<BoxedPropagator, Inconsistency> {
        let id = PropagatorId(self.next_propagator_id);
        self.next_propagator_id += 1;

        let mut propagator: Box<dyn Propagator> = Box::new(propagator);
        propagator.initialise_at_root(&mut PropagatorInitialisationContext::new(
            &mut self.watch_list,
            id,
            &self.assignments,
        ))?;
        self.propagate(&mut propagator)?;

        Ok(propagator)
    }

    pub(crate) fn contains<Var: IntegerVariable>(&self, var: Var, value: i32) -> bool {
        var.contains(&self.assignments, value)
    }

    pub(crate) fn lower_bound(&self, var: DomainId) -> i32 {
        self.assignments.get_lower_bound(var)
    }

    pub(crate) fn get_propagation_context_mut(
        &mut self,
        propagator_id: PropagatorId,
    ) -> PropagationContextMut {
        PropagationContextMut::new(
            &mut self.assignments,
            &mut self.reason_store,
            &mut self.semantic_minimiser,
            propagator_id,
        )
    }

    pub(crate) fn increase_lower_bound_and_notify(
        &mut self,
        propagator: &mut BoxedPropagator,
        id: i32,
        var: DomainId,
        value: i32,
    ) -> EnqueueDecision {
        let result = self.assignments.tighten_lower_bound(var, value, None);
        assert!(result.is_ok(), "The provided value to `increase_lower_bound` caused an empty domain, generally the propagator should not be notified of this change!");
        let context = PropagationContext::new(&self.assignments);
        propagator.notify(
            context,
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

    pub(crate) fn decrease_upper_bound_and_notify(
        &mut self,
        propagator: &mut BoxedPropagator,
        id: i32,
        var: DomainId,
        value: i32,
    ) -> EnqueueDecision {
        let result = self.assignments.tighten_upper_bound(var, value, None);
        assert!(result.is_ok(), "The provided value to `increase_lower_bound` caused an empty domain, generally the propagator should not be notified of this change!");
        let context = PropagationContext::new(&self.assignments);
        propagator.notify(
            context,
            LocalId::from(id as u32),
            OpaqueDomainEvent::from(
                DomainEvents::UPPER_BOUND
                    .get_int_events()
                    .iter()
                    .next()
                    .unwrap(),
            ),
        )
    }
    pub(crate) fn is_literal_false(&self, literal: Literal) -> bool {
        self.assignments
            .evaluate_predicate(literal.get_true_predicate())
            .is_some_and(|truth_value| !truth_value)
    }

    #[allow(dead_code)]
    pub(crate) fn set_lower_bound(&mut self, var: DomainId, bound: i32) -> Result<(), EmptyDomain> {
        self.assignments.tighten_lower_bound(var, bound, None)
    }

    #[allow(dead_code)]
    pub(crate) fn set_upper_bound(&mut self, var: DomainId, bound: i32) -> Result<(), EmptyDomain> {
        self.assignments.tighten_upper_bound(var, bound, None)
    }

    pub(crate) fn upper_bound(&self, var: DomainId) -> i32 {
        self.assignments.get_upper_bound(var)
    }

    pub(crate) fn remove(&mut self, var: DomainId, value: i32) -> Result<(), EmptyDomain> {
        self.assignments.remove_value_from_domain(var, value, None)
    }

    pub(crate) fn set_literal(
        &mut self,
        literal: Literal,
        truth_value: bool,
    ) -> Result<(), EmptyDomain> {
        match truth_value {
            true => self
                .assignments
                .post_predicate(literal.get_true_predicate(), None),
            false => self
                .assignments
                .post_predicate((!literal).get_true_predicate(), None),
        }
    }

    pub(crate) fn propagate(
        &mut self,
        propagator: &mut BoxedPropagator,
    ) -> Result<(), Inconsistency> {
        let context = PropagationContextMut::new(
            &mut self.assignments,
            &mut self.reason_store,
            &mut self.semantic_minimiser,
            PropagatorId(0),
        );
        propagator.propagate(context)
    }

    pub(crate) fn propagate_until_fixed_point(
        &mut self,
        propagator: &mut BoxedPropagator,
    ) -> Result<(), Inconsistency> {
        let mut num_trail_entries = self.assignments.num_trail_entries();
        self.notify_propagator(propagator);
        loop {
            {
                // Specify the life-times to be able to retrieve the trail entries
                let context = PropagationContextMut::new(
                    &mut self.assignments,
                    &mut self.reason_store,
                    &mut self.semantic_minimiser,
                    PropagatorId(0),
                );
                propagator.propagate(context)?;
                self.notify_propagator(propagator);
            }
            if self.assignments.num_trail_entries() == num_trail_entries {
                break;
            }
            num_trail_entries = self.assignments.num_trail_entries();
        }
        Ok(())
    }

    pub(crate) fn notify_propagator(&mut self, propagator: &mut BoxedPropagator) {
        let events = self.assignments.drain_domain_events().collect::<Vec<_>>();
        let context = PropagationContext::new(&self.assignments);
        for (event, domain) in events {
            // The nogood propagator is treated in a special way, since it is not explicitly
            // subscribed to any domain updates, but implicitly is subscribed to all updates.
            if propagator.name() == "NogoodPropagator" {
                let local_id = LocalId::from(domain.id);
                let _ = propagator.notify(context, local_id, event.into());
            } else {
                for propagator_var in self.watch_list.get_affected_propagators(event, domain) {
                    let _ = propagator.notify(context, propagator_var.variable, event.into());
                }
            }
        }
    }

    #[allow(dead_code)]
    pub(crate) fn notify(
        &self,
        propagator: &mut BoxedPropagator,
        event: OpaqueDomainEvent,
        local_id: LocalId,
    ) -> EnqueueDecision {
        propagator.notify(PropagationContext::new(&self.assignments), local_id, event)
    }

    pub(crate) fn get_reason_int(&mut self, predicate: Predicate) -> &PropositionalConjunction {
        let reason_ref = self
            .assignments
            .get_reason_for_predicate_brute_force(predicate);
        let context = PropagationContext::new(&self.assignments);
        self.reason_store
            .get_or_compute(reason_ref, context)
            .expect("reason_ref should not be stale")
    }

    pub(crate) fn get_reason_int_new<'a>(
        &'a mut self,
        predicate: Predicate,
        propagators: &'a mut PropagatorStore,
    ) -> &'a [Predicate] {
        let reason_ref = self
            .assignments
            .get_reason_for_predicate_brute_force(predicate);
        let context = PropagationContext::new(&self.assignments);
        self.reason_store
            .get_or_compute_new(reason_ref, context, propagators)
            .expect("reason_ref should not be stale")
    }

    pub(crate) fn get_reason_bool(
        &mut self,
        literal: Literal,
        truth_value: bool,
    ) -> &PropositionalConjunction {
        let predicate = match truth_value {
            true => literal.get_true_predicate(),
            false => (!literal).get_true_predicate(),
        };
        self.get_reason_int(predicate)
    }

    pub(crate) fn assert_bounds(&self, var: DomainId, lb: i32, ub: i32) {
        let actual_lb = self.lower_bound(var);
        let actual_ub = self.upper_bound(var);

        assert_eq!(
            (lb, ub), (actual_lb, actual_ub),
            "The expected bounds [{lb}..{ub}] did not match the actual bounds [{actual_lb}..{actual_ub}]"
        );
    }
}
