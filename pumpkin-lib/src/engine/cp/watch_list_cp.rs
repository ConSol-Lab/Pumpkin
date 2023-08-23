use crate::{basic_types::DomainId, pumpkin_assert_moderate};

use super::{ConstraintProgrammingPropagator, PropagatorQueue, PropagatorVarId};

#[derive(Default)]
pub struct WatchListCP {
    watchers: Vec<WatcherCP>, //[i] contains propagator ids of propagators that watch domain changes of the i-th integer variable
}

pub struct Watchers<'a> {
    propagator_var: PropagatorVarId,
    watch_list: &'a mut WatchListCP,
}

pub enum DomainEvent {
    Assign,
    Any,
    LowerBound,
    UpperBound,
}

//public functions
impl WatchListCP {
    pub fn grow(&mut self) {
        self.watchers.push(WatcherCP::default());
    }

    pub fn num_integer_variables(&self) -> u32 {
        self.watchers.len() as u32
    }
}

//private functions
impl WatchListCP {
    fn watch_lower_bound_domain_changes(
        &mut self,
        domain: DomainId,
        propagator_var: PropagatorVarId,
    ) {
        pumpkin_assert_moderate!(
            !self.watchers[domain]
                .lower_bound_watchers
                .contains(&propagator_var),
            "Already watching the variable for lower bound changes, for now we consider it an error to request a watch of an already watched variable."
        );

        self.watchers[domain]
            .lower_bound_watchers
            .push(propagator_var);
    }

    fn watch_upper_bound_domain_changes(
        &mut self,
        domain: DomainId,
        propagator_var: PropagatorVarId,
    ) {
        pumpkin_assert_moderate!(
            !self.watchers[domain]
                .upper_bound_watchers
                .contains(&propagator_var),
                "Already watching the variable for upper bound changes, for now we consider it an error to request a watch of an already watched variable."
        );

        self.watchers[domain]
            .upper_bound_watchers
            .push(propagator_var);
    }

    fn watch_hole_domain_changes(&mut self, domain: DomainId, propagator_var: PropagatorVarId) {
        pumpkin_assert_moderate!(
            !self.watchers[domain]
                .hole_watchers
                .contains(&propagator_var),
                "Already watching the variable for hole changes, for now we consider it an error to request a watch of an already watched variable."
        );

        self.watchers[domain].hole_watchers.push(propagator_var);
    }

    fn watch_assign(&mut self, domain: DomainId, propagator_var: PropagatorVarId) {
        pumpkin_assert_moderate!(
            !self.watchers[domain]
                .assign_watchers
                .contains(&propagator_var),
                "Already watching the variable for assignments, for now we consider it an error to request a watch of an already watched variable."
        );

        self.watchers[domain].assign_watchers.push(propagator_var);
    }

    pub fn notify_lower_bound_subscribed_propagators(
        &self,
        integer_variable: DomainId,
        propagators_cp: &mut [Box<dyn ConstraintProgrammingPropagator>],
        propagator_queue: &mut PropagatorQueue,
    ) {
        for &propagator_var in &self.watchers[integer_variable].lower_bound_watchers {
            let propagator = &mut propagators_cp[propagator_var.propagator.0 as usize];
            propagator_queue.enqueue_propagator(propagator_var.propagator, propagator.priority());
        }
    }

    pub fn notify_upper_bound_subscribed_propagators(
        &self,
        integer_variable: DomainId,
        propagators_cp: &mut [Box<dyn ConstraintProgrammingPropagator>],
        propagator_queue: &mut PropagatorQueue,
    ) {
        for &propagator_var in &self.watchers[integer_variable].upper_bound_watchers {
            let propagator = &mut propagators_cp[propagator_var.propagator.0 as usize];
            propagator_queue.enqueue_propagator(propagator_var.propagator, propagator.priority());
        }
    }

    pub fn notify_hole_subscribed_propagators(
        &self,
        integer_variable: DomainId,
        propagators_cp: &mut [Box<dyn ConstraintProgrammingPropagator>],
        propagator_queue: &mut PropagatorQueue,
    ) {
        for &propagator_var in &self.watchers[integer_variable].hole_watchers {
            let propagator = &mut propagators_cp[propagator_var.propagator.0 as usize];
            propagator_queue.enqueue_propagator(propagator_var.propagator, propagator.priority());
        }
    }

    pub fn notify_assign_subscribed_propagators(
        &self,
        integer_variable: DomainId,
        propagators_cp: &mut [Box<dyn ConstraintProgrammingPropagator>],
        propagator_queue: &mut PropagatorQueue,
    ) {
        for &propagator_var in &self.watchers[integer_variable].assign_watchers {
            let propagator = &mut propagators_cp[propagator_var.propagator.0 as usize];
            propagator_queue.enqueue_propagator(propagator_var.propagator, propagator.priority());
        }
    }
}

impl<'a> Watchers<'a> {
    pub(crate) fn new(propagator_var: PropagatorVarId, watch_list: &'a mut WatchListCP) -> Self {
        Watchers {
            propagator_var,
            watch_list,
        }
    }

    pub fn watch(&mut self, domain: DomainId, event: DomainEvent) {
        match event {
            DomainEvent::Any => {
                self.watch_list
                    .watch_hole_domain_changes(domain, self.propagator_var);
                self.watch_list
                    .watch_lower_bound_domain_changes(domain, self.propagator_var);
                self.watch_list
                    .watch_upper_bound_domain_changes(domain, self.propagator_var);
            }
            DomainEvent::LowerBound => self
                .watch_list
                .watch_lower_bound_domain_changes(domain, self.propagator_var),
            DomainEvent::UpperBound => self
                .watch_list
                .watch_upper_bound_domain_changes(domain, self.propagator_var),
            DomainEvent::Assign => self.watch_list.watch_assign(domain, self.propagator_var),
        }
    }
}

#[derive(Default)]
struct WatcherCP {
    pub lower_bound_watchers: Vec<PropagatorVarId>,
    pub upper_bound_watchers: Vec<PropagatorVarId>,
    pub hole_watchers: Vec<PropagatorVarId>,
    pub assign_watchers: Vec<PropagatorVarId>,
}
