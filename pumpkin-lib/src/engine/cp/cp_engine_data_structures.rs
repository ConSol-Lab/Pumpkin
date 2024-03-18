use std::cmp::min;

use crate::basic_types::Predicate;
use crate::basic_types::PropositionalConjunction;
use crate::engine::propagation::EnqueueDecision;
use crate::engine::propagation::PropagationContext;
use crate::engine::propagation::PropagationContextMut;
use crate::engine::propagation::Propagator;
use crate::engine::reason::ReasonRef;
use crate::engine::reason::ReasonStore;
use crate::engine::variables::DomainId;
use crate::engine::AssignmentsInteger;
use crate::engine::AssignmentsPropositional;
use crate::engine::BooleanDomainEvent;
use crate::engine::IntDomainEvent;
use crate::engine::PropagatorQueue;
use crate::engine::WatchListCP;
use crate::engine::WatchListPropositional;
use crate::pumpkin_assert_simple;

#[derive(Debug)]
pub struct CPEngineDataStructures {
    pub assignments_integer: AssignmentsInteger,
    pub watch_list_cp: WatchListCP,
    pub watch_list_propositional: WatchListPropositional,
    pub propagator_queue: PropagatorQueue,

    reason_store: ReasonStore,
    propositional_trail_index: usize,
    event_drain: Vec<(IntDomainEvent, DomainId)>,
}

impl Default for CPEngineDataStructures {
    fn default() -> Self {
        CPEngineDataStructures {
            assignments_integer: AssignmentsInteger::default(),
            watch_list_cp: WatchListCP::default(),
            watch_list_propositional: WatchListPropositional::default(),
            propagator_queue: PropagatorQueue::new(5),
            reason_store: ReasonStore::default(),
            propositional_trail_index: 0,
            event_drain: vec![],
        }
    }
}

impl CPEngineDataStructures {
    pub fn increase_decision_level(&mut self) {
        self.reason_store.increase_decision_level();
        self.assignments_integer.increase_decision_level();
    }

    pub fn new_integer_domain(&mut self, lower_bound: i32, upper_bound: i32) -> DomainId {
        self.watch_list_cp.grow();
        self.assignments_integer.grow(lower_bound, upper_bound)
    }

    /// Method which updates the internal structures of [`CPEngineDataStructures`] upon
    /// backtracking.  This method returns the list of [`DomainId`]s and their values which were
    /// fixed (i.e. domain of size one) before backtracking and are unfixed (i.e. domain of two
    /// or more values) after synchronisation.
    pub fn backtrack(
        &mut self,
        backtrack_level: usize,
        assignment_propositional: &AssignmentsPropositional,
    ) -> Vec<(DomainId, i32)> {
        pumpkin_assert_simple!(
            assignment_propositional.get_decision_level()
                < self.assignments_integer.get_decision_level(),
            "assignments_propositional must be backtracked _before_ CPEngineDataStructures"
        );
        self.propositional_trail_index = min(
            self.propositional_trail_index,
            assignment_propositional.num_trail_entries(),
        );
        let unfixed_variables = self.assignments_integer.synchronise(backtrack_level);
        self.reason_store.synchronise(backtrack_level);
        self.propagator_queue.clear();
        unfixed_variables
    }

    /// Returning `AssignmentsInteger` too is a workaround to allow its usage after a
    ///  call to this. The other option is to inline this method, but then you need to
    ///  expose `self.reason_store` as a public field.
    pub fn compute_reason(
        &mut self,
        reason_ref: ReasonRef,
        assignments_propositional: &AssignmentsPropositional,
    ) -> (&PropositionalConjunction, &AssignmentsInteger) {
        let context = PropagationContext::new(&self.assignments_integer, assignments_propositional);
        let reason = self.reason_store.get_or_compute(reason_ref, &context);
        (
            reason.expect("reason reference should not be stale"),
            &self.assignments_integer,
        )
    }
}

// methods for modifying the domains of variables
//  note that modifying the domain will inform propagators about the changes through the notify
// functions
impl CPEngineDataStructures {
    /// Process the stored domain events. If no events were present, this returns false. Otherwise,
    /// true is returned.
    pub fn process_domain_events(
        &mut self,
        cp_propagators: &mut [Box<dyn Propagator>],
        assignments_propositional: &mut AssignmentsPropositional,
    ) -> bool {
        // If there are no variables being watched then there is no reason to perform these
        // operations
        if self.watch_list_cp.is_watching_anything() {
            self.event_drain
                .extend(self.assignments_integer.drain_domain_events());

            if self.event_drain.is_empty()
                && self.propositional_trail_index == assignments_propositional.num_trail_entries()
            {
                return false;
            }

            for (event, domain) in self.event_drain.drain(..) {
                for propagator_var in self.watch_list_cp.get_affected_propagators(event, domain) {
                    let propagator = &mut cp_propagators[propagator_var.propagator.0 as usize];
                    let mut context = PropagationContextMut::new(
                        &mut self.assignments_integer,
                        &mut self.reason_store,
                        assignments_propositional,
                    );

                    let enqueue_decision =
                        propagator.notify(&mut context, propagator_var.variable, event.into());

                    if enqueue_decision == EnqueueDecision::Enqueue {
                        self.propagator_queue
                            .enqueue_propagator(propagator_var.propagator, propagator.priority());
                    }
                }
            }
        }

        // If there are no literals being watched then there is no reason to perform these
        // operations
        if self.watch_list_propositional.is_watching_anything() {
            for i in self.propositional_trail_index..assignments_propositional.num_trail_entries() {
                let literal = assignments_propositional.get_trail_entry(i);
                for (event, affected_literal) in BooleanDomainEvent::get_iterator(literal) {
                    for propagator_var in self
                        .watch_list_propositional
                        .get_affected_propagators(event, affected_literal)
                    {
                        let propagator = &mut cp_propagators[propagator_var.propagator.0 as usize];
                        let mut context = PropagationContextMut::new(
                            &mut self.assignments_integer,
                            &mut self.reason_store,
                            assignments_propositional,
                        );

                        let enqueue_decision =
                            propagator.notify_literal(&mut context, propagator_var.variable, event);

                        if enqueue_decision == EnqueueDecision::Enqueue {
                            self.propagator_queue.enqueue_propagator(
                                propagator_var.propagator,
                                propagator.priority(),
                            );
                        }
                    }
                }
            }
            self.propositional_trail_index = assignments_propositional.num_trail_entries();
        }

        true
    }

    pub fn create_propagation_context_mut<'a>(
        &'a mut self,
        assignments_propositional: &'a mut AssignmentsPropositional,
    ) -> PropagationContextMut {
        PropagationContextMut::new(
            &mut self.assignments_integer,
            &mut self.reason_store,
            assignments_propositional,
        )
    }

    pub fn create_propagation_context<'a>(
        &'a self,
        assignments_propositional: &'a AssignmentsPropositional,
    ) -> PropagationContext {
        PropagationContext::new(&self.assignments_integer, assignments_propositional)
    }

    pub fn does_predicate_hold(&self, predicate: Predicate) -> bool {
        self.assignments_integer.does_predicate_hold(predicate)
    }
}
