use crate::{
    basic_types::DomainId, propagators::ConstraintProgrammingPropagator, pumpkin_assert_moderate,
};

use super::PropagatorQueue;

#[derive(Default)]
pub struct WatchListCP {
    watchers: Vec<WatcherCP>, //[i] contains propagator ids of propagators that watch domain changes of the i-th integer variable
}

pub struct Watchers<'a> {
    propagator_id: u32,
    watch_list: &'a mut WatchListCP,
}

pub enum DomainEvent {
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

    pub fn add_watches_for_propagator(
        &mut self,
        propagator: &dyn ConstraintProgrammingPropagator,
        propagator_id: u32,
    ) {
        let mut watchers = Watchers {
            propagator_id,
            watch_list: self,
        };

        propagator.register_watches(&mut watchers);
    }

    pub fn get_lower_bound_watchers(&self, integer_variable: DomainId) -> &[u32] {
        &self.watchers[integer_variable].lower_bound_watchers
    }

    pub fn get_upper_bound_watchers(&self, integer_variable: DomainId) -> &[u32] {
        &self.watchers[integer_variable].upper_bound_watchers
    }

    pub fn get_hole_domain_watchers(&self, integer_variable: DomainId) -> &[u32] {
        &self.watchers[integer_variable].hole_watchers
    }
}

//private functions
impl WatchListCP {
    fn watch_lower_bound_domain_changes(&mut self, integer_variable: DomainId, propagator_id: u32) {
        pumpkin_assert_moderate!(
            !self.watchers[integer_variable]
                .lower_bound_watchers
                .contains(&propagator_id),
            "Already watching the variable for lower bound changes, for now we consider it an error to request a watch of an already watched variable."
        );

        self.watchers[integer_variable]
            .lower_bound_watchers
            .push(propagator_id);
    }

    fn watch_upper_bound_domain_changes(&mut self, integer_variable: DomainId, propagator_id: u32) {
        pumpkin_assert_moderate!(
            !self.watchers[integer_variable]
                .upper_bound_watchers
                .contains(&propagator_id),
                "Already watching the variable for upper bound changes, for now we consider it an error to request a watch of an already watched variable."
        );

        self.watchers[integer_variable]
            .upper_bound_watchers
            .push(propagator_id);
    }

    fn watch_hole_domain_changes(&mut self, integer_variable: DomainId, propagator_id: u32) {
        pumpkin_assert_moderate!(
            !self.watchers[integer_variable]
                .hole_watchers
                .contains(&propagator_id),
                "Already watching the variable for hole changes, for now we consider it an error to request a watch of an already watched variable."
        );

        self.watchers[integer_variable]
            .hole_watchers
            .push(propagator_id);
    }

    pub fn notify_lower_bound_subscribed_propagators(
        &self,
        integer_variable: DomainId,
        propagators_cp: &mut [Box<dyn ConstraintProgrammingPropagator>],
        propagator_queue: &mut PropagatorQueue,
    ) {
        for &propagator_id in &self.watchers[integer_variable].lower_bound_watchers {
            let propagator = &mut propagators_cp[propagator_id as usize];
            propagator_queue.enqueue_propagator(propagator_id, propagator.priority());
        }
    }

    pub fn notify_upper_bound_subscribed_propagators(
        &self,
        integer_variable: DomainId,
        propagators_cp: &mut [Box<dyn ConstraintProgrammingPropagator>],
        propagator_queue: &mut PropagatorQueue,
    ) {
        for &propagator_id in &self.watchers[integer_variable].upper_bound_watchers {
            let propagator = &mut propagators_cp[propagator_id as usize];

            propagator_queue.enqueue_propagator(propagator_id, propagator.priority());
        }
    }

    pub fn notify_hole_subscribed_propagators(
        &self,
        integer_variable: DomainId,
        propagators_cp: &mut [Box<dyn ConstraintProgrammingPropagator>],
        propagator_queue: &mut PropagatorQueue,
    ) {
        for &propagator_id in &self.watchers[integer_variable].hole_watchers {
            let propagator = &mut propagators_cp[propagator_id as usize];
            propagator_queue.enqueue_propagator(propagator_id, propagator.priority());
        }
    }
}

impl<'a> Watchers<'a> {
    pub fn watch(&mut self, domain_id: DomainId, event: DomainEvent) {
        match event {
            DomainEvent::Any => {
                self.watch_list
                    .watch_hole_domain_changes(domain_id, self.propagator_id);
                self.watch_list
                    .watch_lower_bound_domain_changes(domain_id, self.propagator_id);
                self.watch_list
                    .watch_upper_bound_domain_changes(domain_id, self.propagator_id);
            }
            DomainEvent::LowerBound => self
                .watch_list
                .watch_lower_bound_domain_changes(domain_id, self.propagator_id),
            DomainEvent::UpperBound => self
                .watch_list
                .watch_upper_bound_domain_changes(domain_id, self.propagator_id),
        }
    }
}

#[derive(Default)]
struct WatcherCP {
    pub lower_bound_watchers: Vec<u32>,
    pub upper_bound_watchers: Vec<u32>,
    pub hole_watchers: Vec<u32>,
}
