mod domain_event_notification;
mod predicate_notification;

pub(crate) use domain_event_notification::domain_events::DomainEvents;
pub(crate) use domain_event_notification::opaque_domain_event::OpaqueDomainEvent;
pub(crate) use domain_event_notification::DomainEvent;
pub(crate) use domain_event_notification::EventSink;
pub(crate) use domain_event_notification::WatchListDomainEvents;
pub(crate) use domain_event_notification::Watchers;
use enumset::EnumSet;
pub(crate) use predicate_notification::PredicateIdAssignments;
pub(crate) use predicate_notification::PredicateNotifier;

use super::propagation::PropagationContext;
use super::propagation::PropagatorVarId;
use crate::basic_types::PredicateId;
use crate::engine::propagation::contexts::PropagationContextWithTrailedValues;
use crate::engine::propagation::store::PropagatorStore;
use crate::engine::propagation::EnqueueDecision;
use crate::engine::propagation::LocalId;
use crate::engine::propagation::PropagatorId;
use crate::engine::Assignments;
use crate::engine::ConstraintSatisfactionSolver;
use crate::engine::PropagatorQueue;
use crate::engine::TrailedValues;
use crate::predicates::Predicate;
use crate::pumpkin_assert_extreme;
use crate::pumpkin_assert_moderate;
use crate::variables::DomainId;

#[derive(Debug)]
pub(crate) struct NotificationEngine {
    /// Responsible for the notification of predicates becoming either falsified or satisfied.
    pub(crate) predicate_notifier: PredicateNotifier,
    /// The trail index for which the last notification took place.
    last_notified_trail_index: usize,
    /// Contains information on which propagator to notify upon
    /// integer events, e.g., lower or upper bound change of a variable.
    watch_list_domain_events: WatchListDomainEvents,
    /// Events which have occurred since the last round of notifications have taken place
    events: EventSink,
    /// Backtrack events which have occurred since the last of backtrack notifications have taken
    /// place
    backtrack_events: EventSink,
}

#[cfg(not(test))]
impl Default for NotificationEngine {
    fn default() -> Self {
        let mut result = Self {
            watch_list_domain_events: Default::default(),
            predicate_notifier: Default::default(),
            last_notified_trail_index: 0,
            events: Default::default(),
            backtrack_events: Default::default(),
        };
        // Grow for the dummy predicate
        result.grow();
        result
    }
}

#[cfg(test)]
impl Default for NotificationEngine {
    fn default() -> Self {
        let watch_list_domain_events = WatchListDomainEvents {
            watchers: Default::default(),
            is_watching_anything: true,
            is_watching_any_backtrack_events: true,
        };

        let mut result = Self {
            watch_list_domain_events,
            predicate_notifier: Default::default(),
            last_notified_trail_index: usize::MAX,
            events: Default::default(),
            backtrack_events: Default::default(),
        };
        // Grow for the dummy predicate
        result.grow();
        result
    }
}

impl NotificationEngine {
    pub(crate) fn debug_empty_clone(&self, capacity: usize) -> Self {
        let mut result = Self {
            predicate_notifier: self.predicate_notifier.debug_empty_clone(),
            ..Default::default()
        };

        for _ in 0..capacity {
            result.grow()
        }
        result
    }

    pub(crate) fn predicate_id_assignments(&self) -> &PredicateIdAssignments {
        &self.predicate_notifier.predicate_id_assignments
    }

    pub(crate) fn predicate_id_assignments_mut(&mut self) -> &mut PredicateIdAssignments {
        &mut self.predicate_notifier.predicate_id_assignments
    }

    pub(crate) fn grow(&mut self) {
        self.watch_list_domain_events.grow();
        self.events.grow();
        self.backtrack_events.grow();
    }

    pub(crate) fn get_id(&mut self, predicate: Predicate) -> PredicateId {
        self.predicate_notifier.predicate_to_id.get_id(predicate)
    }

    pub(crate) fn get_predicate(&mut self, predicate_id: PredicateId) -> Predicate {
        self.predicate_notifier.get_predicate(predicate_id)
    }

    pub(crate) fn watch_all(
        &mut self,
        domain: DomainId,
        events: EnumSet<DomainEvent>,
        propagator_var: PropagatorVarId,
    ) {
        self.watch_list_domain_events.is_watching_anything = true;
        let watcher = &mut self.watch_list_domain_events.watchers[domain];

        for event in events {
            let event_watcher = match event {
                DomainEvent::LowerBound => &mut watcher.forward_watcher.lower_bound_watchers,
                DomainEvent::UpperBound => &mut watcher.forward_watcher.upper_bound_watchers,
                DomainEvent::Assign => &mut watcher.forward_watcher.assign_watchers,
                DomainEvent::Removal => &mut watcher.forward_watcher.removal_watchers,
            };

            if !event_watcher.contains(&propagator_var) {
                event_watcher.push(propagator_var);
            }
        }
    }

    pub(crate) fn watch_all_backtrack(
        &mut self,
        domain: DomainId,
        events: EnumSet<DomainEvent>,
        propagator_var: PropagatorVarId,
    ) {
        self.watch_list_domain_events
            .is_watching_any_backtrack_events = true;
        let watcher = &mut self.watch_list_domain_events.watchers[domain];

        for event in events {
            let backtrack_event_watchers = match event {
                DomainEvent::Assign => &mut watcher.backtrack_watcher.assign_watchers,
                DomainEvent::LowerBound => &mut watcher.backtrack_watcher.lower_bound_watchers,
                DomainEvent::UpperBound => &mut watcher.backtrack_watcher.upper_bound_watchers,
                DomainEvent::Removal => &mut watcher.backtrack_watcher.removal_watchers,
            };

            if !backtrack_event_watchers.contains(&propagator_var) {
                backtrack_event_watchers.push(propagator_var)
            }
        }
    }

    #[allow(
        clippy::too_many_arguments,
        reason = "Should be refactored in the future"
    )]
    /// Returns whether the variable was unfixed
    pub(crate) fn undo_trail_entry(
        &mut self,
        fixed_before: bool,
        lower_bound_before: i32,
        upper_bound_before: i32,
        new_lower_bound: i32,
        new_upper_bound: i32,
        trail_index: usize,
        predicate: Predicate,
    ) {
        if fixed_before
            && new_lower_bound != new_upper_bound
            && self
                .watch_list_domain_events
                .is_watching_any_backtrack_events()
            && trail_index < self.last_notified_trail_index
        {
            // This `domain_id` was unassigned while backtracking
            self.backtrack_events
                .event_occurred(DomainEvent::Assign, predicate.get_domain());
        }

        if self
            .watch_list_domain_events
            .is_watching_any_backtrack_events()
            && trail_index < self.last_notified_trail_index
        {
            // Now we add the remaining events which can occur while backtracking, note that the
            // case of equality has already been handled!
            if lower_bound_before != new_lower_bound {
                self.backtrack_events
                    .event_occurred(DomainEvent::LowerBound, predicate.get_domain())
            }
            if upper_bound_before != new_upper_bound {
                self.backtrack_events
                    .event_occurred(DomainEvent::UpperBound, predicate.get_domain())
            }
            if matches!(
                predicate,
                Predicate::NotEqual {
                    domain_id: _,
                    not_equal_constant: _
                }
            ) {
                self.backtrack_events
                    .event_occurred(DomainEvent::Removal, predicate.get_domain())
            }
        }
    }

    pub(crate) fn clear_events(&mut self) {
        let _ = self.events.drain();
    }

    pub(crate) fn event_occurred(
        &mut self,
        lower_bound_before: i32,
        upper_bound_before: i32,
        new_lower_bound: i32,
        new_upper_bound: i32,
        removal_took_place: bool,
        domain_id: DomainId,
    ) {
        if lower_bound_before != new_lower_bound {
            self.events
                .event_occurred(DomainEvent::LowerBound, domain_id);
        }

        if upper_bound_before != new_upper_bound {
            self.events
                .event_occurred(DomainEvent::UpperBound, domain_id);
        }

        if lower_bound_before != upper_bound_before && new_lower_bound == new_upper_bound {
            self.events.event_occurred(DomainEvent::Assign, domain_id);
        }

        if removal_took_place {
            self.events.event_occurred(DomainEvent::Removal, domain_id);
        }
    }

    /// Process the stored domain events that happens as a result of decision/propagation predicates
    /// to the trail. Propagators are notified and enqueued if needed about the domain events.
    pub(crate) fn notify_propagators_about_domain_events(
        &mut self,
        assignments: &mut Assignments,
        trailed_values: &mut TrailedValues,
        propagators: &mut PropagatorStore,
        propagator_queue: &mut PropagatorQueue,
    ) {
        // Collect so that we can pass the assignments to the methods within the loop
        for (event, domain) in self.events.drain().collect::<Vec<_>>() {
            // First we notify the predicate_notifier that a domain has been updated
            self.predicate_notifier
                .on_update(trailed_values, assignments, event, domain);
            // Special case: the nogood propagator is notified about each event.
            self.notify_nogood_propagator(
                event,
                domain,
                propagators,
                propagator_queue,
                assignments,
                trailed_values,
            );
            // Now notify other propagators subscribed to this event.
            #[allow(clippy::unnecessary_to_owned, reason = "Not unnecessary?")]
            for propagator_var in self
                .watch_list_domain_events
                .get_affected_propagators(event, domain)
                .to_vec()
            {
                let propagator_id = propagator_var.propagator;
                let local_id = propagator_var.variable;
                self.notify_propagator(
                    propagator_id,
                    local_id,
                    event,
                    propagators,
                    propagator_queue,
                    assignments,
                    trailed_values,
                );
            }
        }

        // Then we notify the propagators that a predicate has been satisfied
        self.notify_predicate_id_satisfied(propagators);
        self.notify_predicate_id_falsified(propagators);

        // We assert that the PredicateIdAssignments are the same as the current Assignments
        pumpkin_assert_extreme!(self
            .predicate_notifier
            .predicate_id_assignments
            .predicate_ids()
            .all(|predicate_id| {
                if self
                    .predicate_notifier
                    .predicate_id_assignments
                    .is_untracked(predicate_id)
                {
                    true
                } else {
                    let predicate = self
                        .predicate_notifier
                        .predicate_to_id
                        .get_predicate(predicate_id);
                    match assignments.evaluate_predicate(predicate) {
                        Some(satisfied) => {
                            if satisfied {
                                self.predicate_notifier
                                    .predicate_id_assignments
                                    .is_satisfied(predicate_id)
                            } else {
                                self.predicate_notifier
                                    .predicate_id_assignments
                                    .is_falsified(predicate_id)
                            }
                        }
                        None => self
                            .predicate_notifier
                            .predicate_id_assignments
                            .is_unassigned(predicate_id),
                    }
                }
            }));

        self.last_notified_trail_index = assignments.num_trail_entries();
    }

    pub(crate) fn process_backtrack_events(
        &mut self,
        assignments: &mut Assignments,
        propagators: &mut PropagatorStore,
    ) -> bool {
        // If there are no variables being watched then there is no reason to perform these
        // operations
        if self
            .watch_list_domain_events
            .is_watching_any_backtrack_events()
        {
            if self.backtrack_events.is_empty() {
                return false;
            }

            for (event, domain) in self.backtrack_events.drain().collect::<Vec<_>>() {
                for propagator_var in self
                    .watch_list_domain_events
                    .get_backtrack_affected_propagators(event, domain)
                {
                    let propagator = &mut propagators[propagator_var.propagator];
                    let context = PropagationContext::new(assignments);

                    propagator.notify_backtrack(context, propagator_var.variable, event.into())
                }
            }
        }
        true
    }

    /// Notifies propagators that certain [`Predicate`]s have been falsified.
    ///
    /// Currently, no propagators are informed of this information.
    fn notify_predicate_id_falsified(&mut self, _propagators: &mut PropagatorStore) {
        for _predicate_id in self.predicate_notifier.drain_falsified_predicates() {
            // At the moment this does nothing
        }
    }

    /// Notifies propagators that certain [`Predicate`]s have been satisfied.
    ///
    /// Currently, only the [`NogoodPropagator`] is notified.
    fn notify_predicate_id_satisfied(&mut self, propagators: &mut PropagatorStore) {
        for predicate_id in self.predicate_notifier.drain_satisfied_predicates() {
            let nogood_propagator =
                &mut propagators[ConstraintSatisfactionSolver::get_nogood_propagator_id()];
            nogood_propagator.notify_predicate_id_satisfied(predicate_id);
        }
    }

    fn notify_nogood_propagator(
        &mut self,
        event: DomainEvent,
        domain: DomainId,
        propagators: &mut PropagatorStore,
        propagator_queue: &mut PropagatorQueue,
        assignments: &mut Assignments,
        trailed_values: &mut TrailedValues,
    ) {
        pumpkin_assert_moderate!(
            propagators[ConstraintSatisfactionSolver::get_nogood_propagator_id()].name()
                == "NogoodPropagator"
        );
        let nogood_propagator_id = ConstraintSatisfactionSolver::get_nogood_propagator_id();
        // The nogood propagator is implicitly subscribed to every domain event for every variable.
        // For this reason, its local id matches the domain id.
        // This is special only for the nogood propagator.
        let local_id = LocalId::from(domain.id);
        self.notify_propagator(
            nogood_propagator_id,
            local_id,
            event,
            propagators,
            propagator_queue,
            assignments,
            trailed_values,
        );
    }

    #[allow(clippy::too_many_arguments, reason = "Should be refactored")]
    fn notify_propagator(
        &mut self,
        propagator_id: PropagatorId,
        local_id: LocalId,
        event: DomainEvent,
        propagators: &mut PropagatorStore,
        propagator_queue: &mut PropagatorQueue,
        assignments: &mut Assignments,
        trailed_values: &mut TrailedValues,
    ) {
        let context = PropagationContextWithTrailedValues::new(
            trailed_values,
            assignments,
            self.predicate_id_assignments(),
        );

        let enqueue_decision = propagators[propagator_id].notify(context, local_id, event.into());

        if enqueue_decision == EnqueueDecision::Enqueue {
            propagator_queue
                .enqueue_propagator(propagator_id, propagators[propagator_id].priority());
        }
    }

    pub(crate) fn update_last_notified_index(&mut self, assignments: &mut Assignments) {
        self.last_notified_trail_index = assignments.num_trail_entries();
    }

    pub(crate) fn clear_event_drain(&mut self) {
        let _ = self.events.drain();
    }

    pub(crate) fn track_predicate(
        &mut self,
        predicate: PredicateId,
        trailed_values: &mut TrailedValues,
        assignments: &Assignments,
    ) {
        self.predicate_notifier
            .track_predicate(predicate, trailed_values, assignments)
    }

    #[cfg(test)]
    pub(crate) fn drain_backtrack_domain_events(
        &mut self,
    ) -> impl Iterator<Item = (DomainEvent, DomainId)> + '_ {
        self.backtrack_events.drain()
    }

    #[cfg(test)]
    pub(crate) fn drain_domain_events(
        &mut self,
    ) -> impl Iterator<Item = (DomainEvent, DomainId)> + '_ {
        self.events.drain()
    }

    #[cfg(test)]
    /// Process the stored domain events that happens as a result of decision/propagation predicates
    /// to the trail. Propagators are notified and enqueued if needed about the domain events.
    pub(crate) fn notify_propagators_about_domain_events_test(
        &mut self,
        assignments: &mut Assignments,
        trailed_values: &mut TrailedValues,
        propagators: &mut PropagatorStore,
        propagator_queue: &mut PropagatorQueue,
    ) {
        // Collect so that we can pass the assignments to the methods within the loop
        for (event, domain) in self.events.drain().collect::<Vec<_>>() {
            // First we notify the predicate_notifier that a domain has been updated
            self.predicate_notifier
                .on_update(trailed_values, assignments, event, domain);

            // Now notify other propagators subscribed to this event.
            #[allow(clippy::unnecessary_to_owned, reason = "Not unnecessary?")]
            for propagator_var in self
                .watch_list_domain_events
                .get_affected_propagators(event, domain)
                .to_vec()
            {
                let propagator_id = propagator_var.propagator;
                let local_id = propagator_var.variable;
                self.notify_propagator(
                    propagator_id,
                    local_id,
                    event,
                    propagators,
                    propagator_queue,
                    assignments,
                    trailed_values,
                );
            }
        }

        // Then we notify the propagators that a predicate has been satisfied
        self.notify_predicate_id_satisfied(propagators);
        self.notify_predicate_id_falsified(propagators);

        self.last_notified_trail_index = assignments.num_trail_entries();
    }

    pub(crate) fn num_predicate_ids(&self) -> usize {
        self.predicate_notifier.predicate_to_id.num_predicate_ids()
    }
}
