use crate::{
    basic_types::{EnqueueStatus, IntegerVariable, PropagatorIdentifier},
    propagators::ConstraintProgrammingPropagator,
    pumpkin_asserts::pumpkin_assert_moderate,
};

use super::{AssignmentsInteger, DomainManager, PropagatorQueue};

pub struct WatchListCP {
    watchers: Vec<WatcherCP>, //[i] contains propagator ids of propagators that watch domain changes of the i-th integer variable
}

//public functions
impl WatchListCP {
    pub fn new() -> WatchListCP {
        WatchListCP { watchers: vec![] }
    }

    pub fn grow(&mut self) {
        self.watchers.push(WatcherCP::default());
    }

    pub fn num_integer_variables(&self) -> u32 {
        self.watchers.len() as u32
    }

    pub fn add_watches_for_propagator(
        &mut self,
        propagator: &dyn ConstraintProgrammingPropagator,
        propagator_identifier: PropagatorIdentifier,
    ) {
        for integer_variable in propagator.get_integer_variables_to_watch_for_lower_bound_changes()
        {
            self.watch_lower_bound_domain_changes(integer_variable, propagator_identifier);
        }

        for integer_variable in propagator.get_integer_variables_to_watch_for_upper_bound_changes()
        {
            self.watch_upper_bound_domain_changes(integer_variable, propagator_identifier);
        }

        for integer_variable in propagator.get_integer_variables_to_watch_for_domain_hole_changes()
        {
            self.watch_hole_domain_changes(integer_variable, propagator_identifier);
        }
    }

    pub fn get_lower_bound_watchers(
        &self,
        integer_variable: IntegerVariable,
    ) -> &[PropagatorIdentifier] {
        &self.watchers[integer_variable].lower_bound_watchers
    }

    pub fn get_upper_bound_watchers(
        &self,
        integer_variable: IntegerVariable,
    ) -> &[PropagatorIdentifier] {
        &self.watchers[integer_variable].upper_bound_watchers
    }

    pub fn get_hole_domain_watchers(
        &self,
        integer_variable: IntegerVariable,
    ) -> &[PropagatorIdentifier] {
        &self.watchers[integer_variable].hole_watchers
    }
}

//private functions
impl WatchListCP {
    fn watch_lower_bound_domain_changes(
        &mut self,
        integer_variable: IntegerVariable,
        propagator_id: PropagatorIdentifier,
    ) {
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

    fn watch_upper_bound_domain_changes(
        &mut self,
        integer_variable: IntegerVariable,
        propagator_id: PropagatorIdentifier,
    ) {
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

    fn watch_hole_domain_changes(
        &mut self,
        integer_variable: IntegerVariable,
        propagator_id: PropagatorIdentifier,
    ) {
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
        integer_variable: IntegerVariable,
        old_lower_bound: i32,
        new_lower_bound: i32,
        propagators_cp: &mut [Box<dyn ConstraintProgrammingPropagator>],
        propagator_queue: &mut PropagatorQueue,
        assignments_integer: &mut AssignmentsInteger,
    ) {
        for propagator_identifier in &self.watchers[integer_variable].lower_bound_watchers {
            let propagator = &mut propagators_cp[propagator_identifier.id as usize];
            let domains =
                DomainManager::new(propagator_identifier.id as usize, assignments_integer);

            let enqueue_status = propagator.notify_lower_bound_integer_variable_change(
                integer_variable,
                old_lower_bound,
                new_lower_bound,
                &domains,
            );

            if let EnqueueStatus::ShouldEnqueue = enqueue_status {
                propagator_queue.enqueue_propagator(*propagator_identifier, propagator.priority());
            }
        }
    }

    pub fn notify_upper_bound_subscribed_propagators(
        &self,
        integer_variable: IntegerVariable,
        old_upper_bound: i32,
        new_upper_bound: i32,
        propagators_cp: &mut [Box<dyn ConstraintProgrammingPropagator>],
        propagator_queue: &mut PropagatorQueue,
        assignments_integer: &mut AssignmentsInteger,
    ) {
        for propagator_identifier in &self.watchers[integer_variable].upper_bound_watchers {
            let propagator = &mut propagators_cp[propagator_identifier.id as usize];
            let domains =
                DomainManager::new(propagator_identifier.id as usize, assignments_integer);

            let enqueue_status = propagator.notify_upper_bound_integer_variable_change(
                integer_variable,
                old_upper_bound,
                new_upper_bound,
                &domains,
            );

            if let EnqueueStatus::ShouldEnqueue = enqueue_status {
                propagator_queue.enqueue_propagator(*propagator_identifier, propagator.priority());
            }
        }
    }

    pub fn notify_hole_subscribed_propagators(
        &self,
        integer_variable: IntegerVariable,
        removed_value_from_domain: i32,
        propagators_cp: &mut [Box<dyn ConstraintProgrammingPropagator>],
        propagator_queue: &mut PropagatorQueue,
        assignments_integer: &mut AssignmentsInteger,
    ) {
        for propagator_identifier in &self.watchers[integer_variable].hole_watchers {
            let propagator = &mut propagators_cp[propagator_identifier.id as usize];
            let domains =
                DomainManager::new(propagator_identifier.id as usize, assignments_integer);

            let enqueue_status = propagator.notify_domain_hole_integer_variable_change(
                integer_variable,
                removed_value_from_domain,
                &domains,
            );

            if let EnqueueStatus::ShouldEnqueue = enqueue_status {
                propagator_queue.enqueue_propagator(*propagator_identifier, propagator.priority());
            }
        }
    }
}

#[derive(Default)]
struct WatcherCP {
    pub lower_bound_watchers: Vec<PropagatorIdentifier>,
    pub upper_bound_watchers: Vec<PropagatorIdentifier>,
    pub hole_watchers: Vec<PropagatorIdentifier>,
}
