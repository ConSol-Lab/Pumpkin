use crate::basic_types::{DomainId, Predicate};

use super::{
    AssignmentsInteger, ConstraintProgrammingPropagator, DomainEvent, EmptyDomain, EnqueueDecision,
    PropagationContext, PropagatorQueue, PropagatorVarId, WatchListCP,
};

pub struct CPEngineDataStructures {
    pub assignments_integer: AssignmentsInteger,
    pub watch_list_cp: WatchListCP,
    pub propagator_queue: PropagatorQueue,

    event_drain: Vec<(DomainEvent, DomainId)>,
}

impl Default for CPEngineDataStructures {
    fn default() -> Self {
        CPEngineDataStructures {
            assignments_integer: AssignmentsInteger::default(),
            watch_list_cp: WatchListCP::default(),
            propagator_queue: PropagatorQueue::new(5),
            event_drain: vec![],
        }
    }
}

impl CPEngineDataStructures {
    pub fn backtrack(&mut self, backtrack_level: u32) {
        self.assignments_integer.synchronise(backtrack_level);
        self.propagator_queue.clear();
    }
}

//methods for motifying the domains of variables
//  note that modifying the domain will inform propagators about the changes through the notify functions
impl CPEngineDataStructures {
    /// Process the stored domain events. If no events were present, this returns false. Otherwise,
    /// true is returned.
    pub fn process_domain_events(
        &mut self,
        cp_propagators: &mut [Box<dyn ConstraintProgrammingPropagator>],
    ) -> bool {
        self.event_drain
            .extend(self.assignments_integer.drain_domain_events());

        if self.event_drain.is_empty() {
            return false;
        }

        for (event, domain) in self.event_drain.drain(..) {
            for propagator_var in self.watch_list_cp.get_affected_propagators(event, domain) {
                let propagator = &mut cp_propagators[propagator_var.propagator.0 as usize];
                let mut context = PropagationContext::new(
                    &mut self.assignments_integer,
                    propagator_var.propagator,
                );

                let enqueue_decision =
                    propagator.notify(&mut context, propagator_var.variable, event.into());

                if enqueue_decision == EnqueueDecision::Enqueue {
                    self.propagator_queue
                        .enqueue_propagator(propagator_var.propagator, propagator.priority());
                }
            }
        }

        true
    }

    //changes the domains according to the predicate
    //  in case the predicate is already true, no changes happen
    //  however in case the predicate would lead to inconsistent domains, e.g., decreasing the upper bound past the lower bound
    //      pumpkin asserts will make the program crash
    pub fn apply_predicate(
        &mut self,
        predicate: &Predicate,
        propagator_reason: Option<PropagatorVarId>,
    ) -> Result<(), EmptyDomain> {
        if self.does_predicate_hold(predicate) {
            return Ok(());
        }

        match *predicate {
            Predicate::LowerBound {
                domain_id,
                lower_bound,
            } => self.assignments_integer.tighten_lower_bound(
                domain_id,
                lower_bound,
                propagator_reason,
            ),
            Predicate::UpperBound {
                domain_id,
                upper_bound,
            } => self.assignments_integer.tighten_upper_bound(
                domain_id,
                upper_bound,
                propagator_reason,
            ),
            Predicate::NotEqual {
                domain_id,
                not_equal_constant,
            } => self.assignments_integer.remove_value_from_domain(
                domain_id,
                not_equal_constant,
                propagator_reason,
            ),
            Predicate::Equal {
                domain_id,
                equality_constant,
            } => self.assignments_integer.make_assignment(
                domain_id,
                equality_constant,
                propagator_reason,
            ),
        }
    }

    pub fn does_predicate_hold(&self, predicate: &Predicate) -> bool {
        self.assignments_integer.does_predicate_hold(predicate)
    }
}
