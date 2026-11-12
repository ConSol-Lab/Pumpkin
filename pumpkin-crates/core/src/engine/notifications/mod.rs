mod domain_event_notification;
mod predicate_notification;

pub use domain_event_notification::DomainEvent;
pub(crate) use domain_event_notification::EventSink;
pub(crate) use domain_event_notification::WatchListDomainEvents;
pub(crate) use domain_event_notification::Watchers;
pub use domain_event_notification::domain_events::DomainEvents;
pub use domain_event_notification::opaque_domain_event::OpaqueDomainEvent;
use enumset::EnumSet;
pub(crate) use predicate_notification::PredicateNotifier;

use crate::basic_types::PredicateId;
use crate::basic_types::Trail;
use crate::containers::KeyedVec;
use crate::engine::Assignments;
use crate::engine::PropagatorQueue;
use crate::engine::TrailedValues;
use crate::predicates::Predicate;
use crate::propagation::Domains;
use crate::propagation::EnqueueDecision;
use crate::propagation::LocalId;
use crate::propagation::NotificationContext;
#[cfg(doc)]
use crate::propagation::Propagator;
use crate::propagation::PropagatorId;
use crate::propagation::PropagatorVarId;
use crate::propagation::store::PropagatorStore;
use crate::pumpkin_assert_extreme;
use crate::pumpkin_assert_simple;
use crate::variables::DomainId;

/// A watcher which is notified when a predicate becomes true.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum PredicateWatcher {
    /// The propagator is notified through [`Propagator::notify_predicate_id_satisfied`].
    Propagator(PropagatorId),
    /// The propagator is notified through [`Propagator::notify`] for the predicate used as a 0-1
    /// variable with the given event.
    Variable(PropagatorVarId, DomainEvent),
}

#[derive(Debug, Clone)]
pub(crate) struct NotificationEngine {
    /// Responsible for the notification of predicates becoming either falsified or satisfied.
    pub(crate) predicate_notifier: PredicateNotifier,
    /// The trail index for which the last notification took place.
    last_notified_trail_index: usize,
    /// Contains information on which propagator to notify upon
    /// integer events, e.g., lower or upper bound change of a variable.
    watch_list_domain_events: WatchListDomainEvents,
    /// The watch list from predicates to propagators, which are notified when the predicate
    /// becomes true.
    watch_list_predicate_id: KeyedVec<PredicateId, Vec<PredicateWatcher>>,
    /// The watch list from predicates to the propagator variables which are notified when the
    /// predicate is no longer true after backtracking, together with the event to notify them of.
    backtrack_watch_list_predicate_id: KeyedVec<PredicateId, Vec<(PropagatorVarId, DomainEvent)>>,
    /// The predicates with backtrack watchers which are true, per checkpoint at which they were
    /// added.
    ///
    /// When backtracking over that checkpoint, the backtrack watchers of a predicate which is no
    /// longer true are notified.
    satisfied_backtrack_watched_predicates: Trail<PredicateId>,
    /// Events which have occurred since the last round of notifications have taken place
    events: EventSink,
    /// Backtrack events which have occurred since the last of backtrack notifications have taken
    /// place
    backtrack_events: EventSink,
}

impl Default for NotificationEngine {
    fn default() -> Self {
        let mut result = Self {
            watch_list_domain_events: Default::default(),
            watch_list_predicate_id: Default::default(),
            backtrack_watch_list_predicate_id: Default::default(),
            satisfied_backtrack_watched_predicates: Default::default(),
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

impl NotificationEngine {
    #[cfg(test)]
    pub(crate) fn test_default() -> Self {
        let watch_list_domain_events = WatchListDomainEvents {
            watchers: Default::default(),
            is_watching_anything: true,
            is_watching_any_backtrack_events: true,
        };

        let mut result = Self {
            watch_list_domain_events,
            watch_list_predicate_id: Default::default(),
            backtrack_watch_list_predicate_id: Default::default(),
            satisfied_backtrack_watched_predicates: Default::default(),
            predicate_notifier: Default::default(),
            last_notified_trail_index: usize::MAX,
            events: Default::default(),
            backtrack_events: Default::default(),
        };
        // Grow for the dummy predicate
        result.grow();
        result
    }

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

    pub(crate) fn grow(&mut self) {
        self.watch_list_domain_events.grow();
        self.events.grow();
        self.backtrack_events.grow();
    }

    pub(crate) fn get_id(&mut self, predicate: Predicate) -> PredicateId {
        self.predicate_notifier.predicate_to_id.get_id(predicate)
    }

    pub(crate) fn get_predicate(&self, predicate_id: PredicateId) -> Predicate {
        self.predicate_notifier.get_predicate(predicate_id)
    }

    pub(crate) fn register(
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

    pub(crate) fn unwatch_all(&mut self, domain: DomainId, propagator_var: PropagatorVarId) {
        let watchers = &mut self.watch_list_domain_events.watchers[domain];

        let event_watchers = [
            &mut watchers.forward_watcher.lower_bound_watchers,
            &mut watchers.forward_watcher.upper_bound_watchers,
            &mut watchers.forward_watcher.assign_watchers,
            &mut watchers.forward_watcher.removal_watchers,
        ];

        for event_watcher in event_watchers {
            if let Some(index) = event_watcher.iter().position(|pv| *pv == propagator_var) {
                let _ = event_watcher.swap_remove(index);
            }
        }
    }

    pub(crate) fn watch_predicate(
        &mut self,
        predicate: Predicate,
        propagator_id: PropagatorId,
        trailed_values: &mut TrailedValues,
        assignments: &Assignments,
    ) -> PredicateId {
        let predicate_id = self.get_id(predicate);
        self.watch_predicate_id(predicate_id, propagator_id, trailed_values, assignments);

        predicate_id
    }

    pub(crate) fn watch_predicate_id(
        &mut self,
        predicate_id: PredicateId,
        propagator_id: PropagatorId,
        trailed_values: &mut TrailedValues,
        assignments: &Assignments,
    ) {
        self.watch_list_predicate_id
            .accomodate(predicate_id, vec![]);
        self.watch_list_predicate_id[predicate_id]
            .push(PredicateWatcher::Propagator(propagator_id));

        self.predicate_notifier.track_predicate(
            predicate_id,
            trailed_values,
            assignments,
            self.last_notified_trail_index,
        );
    }

    pub(crate) fn unwatch_predicate(
        &mut self,
        predicate_id: PredicateId,
        propagator_to_unwatch: PropagatorId,
    ) {
        let watch_list = &mut self.watch_list_predicate_id[predicate_id];

        let index = watch_list
            .iter()
            .position(|&watcher| watcher == PredicateWatcher::Propagator(propagator_to_unwatch))
            .unwrap_or_else(|| {
                panic!("cannot unwatch ({predicate_id:?}, {propagator_to_unwatch:?}) pair if it was not watched")
            });

        let _ = watch_list.swap_remove(index);

        // TODO: Can we remove the predicate from being tracked if it does not have watchers?
    }

    /// Watch the events of the given [`Predicate`] used as a 0-1 variable.
    pub(crate) fn watch_predicate_events(
        &mut self,
        predicate: Predicate,
        events: EnumSet<DomainEvent>,
        propagator_var: PropagatorVarId,
        trailed_values: &mut TrailedValues,
        assignments: &Assignments,
    ) {
        // A predicate which is assigned at the root never triggers an event, so it does not need
        // to be watched.
        if assignments.is_assigned_at_checkpoint(predicate, 0) {
            return;
        }

        for (predicate, event) in predicate_event_watches(predicate, events) {
            let predicate_id = self.get_id(predicate);
            self.watch_list_predicate_id
                .accomodate(predicate_id, vec![]);
            self.watch_list_predicate_id[predicate_id]
                .push(PredicateWatcher::Variable(propagator_var, event));

            self.predicate_notifier.track_predicate(
                predicate_id,
                trailed_values,
                assignments,
                self.last_notified_trail_index,
            );
        }
    }

    /// Stop watching the events of the given [`Predicate`] used as a 0-1 variable.
    pub(crate) fn unwatch_predicate_events(
        &mut self,
        predicate: Predicate,
        propagator_var: PropagatorVarId,
    ) {
        for predicate in [predicate, !predicate] {
            let predicate_id = self.get_id(predicate);
            if let Some(watch_list) = self.watch_list_predicate_id.get_mut(predicate_id) {
                watch_list.retain(|watcher| {
                    !matches!(watcher, PredicateWatcher::Variable(var, _) if *var == propagator_var)
                });
            }
        }
    }

    /// Watch the events of the given [`Predicate`] used as a 0-1 variable being undone when
    /// backtracking.
    pub(crate) fn watch_predicate_events_backtrack(
        &mut self,
        predicate: Predicate,
        events: EnumSet<DomainEvent>,
        propagator_var: PropagatorVarId,
        trailed_values: &mut TrailedValues,
        assignments: &Assignments,
    ) {
        // See `NotificationEngine::watch_predicate_events`.
        if assignments.is_assigned_at_checkpoint(predicate, 0) {
            return;
        }

        for (predicate, event) in predicate_event_watches(predicate, events) {
            let predicate_id = self.get_id(predicate);
            self.add_if_already_true(predicate_id, predicate, assignments);
            self.backtrack_watch_list_predicate_id
                .accomodate(predicate_id, vec![]);
            self.backtrack_watch_list_predicate_id[predicate_id].push((propagator_var, event));

            self.predicate_notifier.track_predicate(
                predicate_id,
                trailed_values,
                assignments,
                self.last_notified_trail_index,
            );
        }
    }

    /// Adds `predicate` to [`Self::satisfied_backtrack_watched_predicates`] if it is true and has
    /// no backtrack watchers yet, so that the backtrack watcher which is added now is notified when
    /// it is undone.
    ///
    /// A predicate which became true by an update that has already been processed is not reported
    /// as satisfied (see [`PredicateNotifier::track_predicate`]), so it is added here.
    /// A predicate which became true by an update that has not been processed yet is added when it
    /// is reported, and a predicate with backtrack watchers is already added.
    ///
    /// For example, let `[x >= 6]` be posted at checkpoint 1, and let a propagator watch
    /// `[x >= 5]` being undone at checkpoint 2.
    /// Then `[x >= 5]` is added here, and its backtrack watchers are notified only when
    /// backtracking to the root undoes `[x >= 6]`.
    fn add_if_already_true(
        &mut self,
        predicate_id: PredicateId,
        predicate: Predicate,
        assignments: &Assignments,
    ) {
        let has_backtrack_watchers = self
            .backtrack_watch_list_predicate_id
            .get(predicate_id)
            .is_some_and(|watch_list| !watch_list.is_empty());
        let is_processed = assignments
            .get_trail_position(&predicate)
            .is_none_or(|trail_position| trail_position < self.last_notified_trail_index);

        // The predicate is added at the current checkpoint, which can be later than the checkpoint
        // at which it became true; `process_backtrack_events` handles this when backtracking.
        if !has_backtrack_watchers
            && assignments.evaluate_predicate(predicate) == Some(true)
            && is_processed
        {
            self.satisfied_backtrack_watched_predicates
                .push(predicate_id);
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
            if predicate.is_not_equal_predicate() {
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
        assignments: &Assignments,
        trailed_values: &mut TrailedValues,
        propagators: &mut PropagatorStore,
        propagator_queue: &mut PropagatorQueue,
    ) {
        // We first take the events because otherwise we get mutability issues when calling methods
        // on self
        let mut events = std::mem::take(&mut self.events);

        for (event, domain) in events.drain() {
            // First we notify the predicate_notifier that a domain has been updated
            self.predicate_notifier
                .on_update(trailed_values, assignments, event, domain);
            // Now notify other propagators subscribed to this event.
            #[allow(clippy::unnecessary_to_owned, reason = "Not unnecessary?")]
            for propagator_var in self
                .watch_list_domain_events
                .get_affected_propagators(event, domain)
            {
                let propagator_id = propagator_var.propagator;
                let local_id = propagator_var.variable;
                Self::notify_propagator(
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

        self.events = events;

        // Then we notify the propagators that a predicate has been satisfied.
        self.notify_predicate_id_satisfied(
            propagators,
            propagator_queue,
            trailed_values,
            assignments,
        );

        self.last_notified_trail_index = assignments.num_trail_entries();
    }

    pub(crate) fn process_backtrack_events(
        &mut self,
        new_checkpoint: usize,
        assignments: &Assignments,
        trailed_values: &mut TrailedValues,
        propagators: &mut PropagatorStore,
    ) {
        // Whether a predicate is still true is read from the assignments, so they must be
        // backtracked first.
        pumpkin_assert_simple!(
            assignments.get_checkpoint() == new_checkpoint,
            "Expected the assignments to have been backtracked previously"
        );

        // The predicates are collected, since predicates which are still true are added to the
        // trail again.
        let backtracked_predicates = self
            .satisfied_backtrack_watched_predicates
            .synchronise(new_checkpoint)
            .collect::<Vec<_>>();

        for predicate_id in backtracked_predicates {
            let predicate = self.predicate_notifier.get_predicate(predicate_id);
            if assignments.evaluate_predicate(predicate) == Some(true) {
                // The predicate was assigned before the checkpoint at which it was added, which
                // happens when it was already true when it was first watched.
                self.satisfied_backtrack_watched_predicates
                    .push(predicate_id);
                continue;
            }

            for &(propagator_var, event) in &self.backtrack_watch_list_predicate_id[predicate_id] {
                let propagator = &mut propagators[propagator_var.propagator];
                let context = Domains::new(assignments, trailed_values);

                propagator.notify_backtrack(context, propagator_var.variable, event.into())
            }
        }

        // If there are no variables being watched then there is no reason to perform these
        // operations
        if self
            .watch_list_domain_events
            .is_watching_any_backtrack_events()
            && !self.backtrack_events.is_empty()
        {
            for (event, domain) in self.backtrack_events.drain().collect::<Vec<_>>() {
                for propagator_var in self
                    .watch_list_domain_events
                    .get_backtrack_affected_propagators(event, domain)
                {
                    let propagator = &mut propagators[propagator_var.propagator];
                    let context = Domains::new(assignments, trailed_values);

                    propagator.notify_backtrack(context, propagator_var.variable, event.into())
                }
            }
        }
    }

    /// Notifies the propagator that certain [`Predicate`]s have been satisfied.
    fn notify_predicate_id_satisfied(
        &mut self,
        propagators: &mut PropagatorStore,
        propagator_queue: &mut PropagatorQueue,
        trailed_values: &mut TrailedValues,
        assignments: &Assignments,
    ) {
        for predicate_id in self.predicate_notifier.drain_satisfied_predicates() {
            if let Some(watch_list) = self.watch_list_predicate_id.get(predicate_id) {
                for &watcher in watch_list {
                    match watcher {
                        PredicateWatcher::Propagator(propagator_id) => {
                            let mut context = NotificationContext::new(trailed_values, assignments);

                            let propagator = &mut propagators[propagator_id];
                            let enqueue_decision = propagator
                                .notify_predicate_id_satisfied(context.reborrow(), predicate_id);

                            if enqueue_decision == EnqueueDecision::Enqueue {
                                propagator_queue
                                    .enqueue_propagator(propagator_id, propagator.priority());
                            }
                        }
                        PredicateWatcher::Variable(propagator_var, event) => {
                            Self::notify_propagator(
                                propagator_var.propagator,
                                propagator_var.variable,
                                event,
                                propagators,
                                propagator_queue,
                                assignments,
                                trailed_values,
                            );
                        }
                    }
                }
            }

            // The predicate becomes true at the current checkpoint, so it is undone when
            // backtracking over this checkpoint.
            if self
                .backtrack_watch_list_predicate_id
                .get(predicate_id)
                .is_some_and(|watch_list| !watch_list.is_empty())
            {
                self.satisfied_backtrack_watched_predicates
                    .push(predicate_id);
            }
        }
    }

    #[allow(clippy::too_many_arguments, reason = "Should be refactored")]
    fn notify_propagator(
        propagator_id: PropagatorId,
        local_id: LocalId,
        event: DomainEvent,
        propagators: &mut PropagatorStore,
        propagator_queue: &mut PropagatorQueue,
        assignments: &Assignments,
        trailed_values: &mut TrailedValues,
    ) {
        let context = NotificationContext::new(trailed_values, assignments);

        let enqueue_decision = propagators[propagator_id].notify(context, local_id, event.into());

        if enqueue_decision == EnqueueDecision::Enqueue {
            propagator_queue
                .enqueue_propagator(propagator_id, propagators[propagator_id].priority());
        }
    }

    pub(crate) fn update_last_notified_index(&mut self, assignments: &Assignments) {
        self.last_notified_trail_index = assignments.num_trail_entries();
    }

    pub(crate) fn clear_event_drain(&mut self) {
        let _ = self.events.drain();
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

    #[deprecated]
    /// Process the stored domain events that happens as a result of decision/propagation predicates
    /// to the trail. Propagators are notified and enqueued if needed about the domain events.
    pub(crate) fn notify_propagators_about_domain_events_test(
        &mut self,
        assignments: &Assignments,
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
                Self::notify_propagator(
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

        self.notify_predicate_id_satisfied(
            propagators,
            propagator_queue,
            trailed_values,
            assignments,
        );

        self.last_notified_trail_index = assignments.num_trail_entries();
    }

    pub(crate) fn num_predicate_ids(&self) -> usize {
        self.predicate_notifier.predicate_to_id.num_predicate_ids()
    }

    pub(crate) fn new_checkpoint(&mut self) {
        self.predicate_notifier
            .predicate_id_assignments
            .new_checkpoint();
        self.satisfied_backtrack_watched_predicates.new_checkpoint();
    }

    pub(crate) fn debug_create_from_assignments(&mut self, assignments: &Assignments) {
        self.predicate_notifier
            .debug_create_from_assignments(assignments);
    }

    /// Returns whether the [`Predicate`] corresponding to the provided [`PredicateId`] is
    /// satisfied.
    pub(crate) fn is_predicate_id_satisfied(
        &mut self,
        predicate_id: PredicateId,
        assignments: &Assignments,
    ) -> bool {
        let result = self
            .predicate_notifier
            .predicate_id_assignments
            .is_satisfied(
                predicate_id,
                assignments,
                &self.predicate_notifier.predicate_to_id,
            );
        pumpkin_assert_extreme!(
            {
                let predicate = self.get_predicate(predicate_id);

                assignments.is_predicate_satisfied(predicate) == result
            },
            "Expected status of predicate ID to be the same as the one stored in the Assignments"
        );
        result
    }

    /// Returns whether the [`Predicate`] corresponding to the provided [`PredicateId`] is
    /// satisfied.
    pub(crate) fn evaluate_predicate_id(
        &mut self,
        predicate_id: PredicateId,
        assignments: &Assignments,
    ) -> Option<bool> {
        self.predicate_notifier.predicate_id_assignments.evaluate(
            predicate_id,
            assignments,
            &self.predicate_notifier.predicate_to_id,
        )
    }

    /// Returns whether the [`Predicate`] corresponding to the provided [`PredicateId`] is
    /// falsified.
    pub(crate) fn is_predicate_id_falsified(
        &mut self,
        predicate_id: PredicateId,
        assignments: &Assignments,
    ) -> bool {
        let result = self
            .predicate_notifier
            .predicate_id_assignments
            .is_falsified(
                predicate_id,
                assignments,
                &self.predicate_notifier.predicate_to_id,
            );

        pumpkin_assert_extreme!(
            {
                let predicate = self.get_predicate(predicate_id);

                assignments.is_predicate_falsified(predicate) == result
            },
            "Expected status of predicate ID to be the same as the one stored in the Assignments"
        );

        result
    }

    pub(crate) fn synchronise(
        &mut self,
        backtrack_level: usize,
        assignments: &Assignments,
        _trailed_values: &mut TrailedValues,
    ) {
        pumpkin_assert_simple!(
            assignments.get_checkpoint() == backtrack_level,
            "Expected the assignments to have been backtracked previously"
        );
        self.predicate_notifier
            .predicate_id_assignments
            .synchronise(
                backtrack_level,
                assignments,
                &self.predicate_notifier.predicate_to_id,
            )
    }
}

/// For `predicate` used as a 0-1 variable and registered for the given events, returns the
/// predicates whose satisfaction triggers an event, together with the event which is triggered.
///
/// As for a 0-1 domain, `predicate` becoming true triggers the lower-bound and assign events, and
/// `predicate` becoming false (i.e., `!predicate` becoming true) triggers the upper-bound and
/// assign events. The assign event is thus watched on both
/// predicates, but only one of them can become true. The removal event is never triggered, since
/// a 0-1 domain has no values between its bounds.
fn predicate_event_watches(
    predicate: Predicate,
    events: EnumSet<DomainEvent>,
) -> impl Iterator<Item = (Predicate, DomainEvent)> {
    [
        (predicate, DomainEvent::LowerBound),
        (predicate, DomainEvent::Assign),
        (!predicate, DomainEvent::UpperBound),
        (!predicate, DomainEvent::Assign),
    ]
    .into_iter()
    .filter(move |&(_, event)| events.contains(event))
}

#[cfg(test)]
mod tests {
    use crate::basic_types::PredicateId;
    use crate::engine::PropagationStatusCP;
    use crate::engine::State;
    use crate::predicate;
    use crate::predicates::Predicate;
    use crate::propagation::EnqueueDecision;
    use crate::propagation::EventsToRegister;
    use crate::propagation::NotificationContext;
    use crate::propagation::PropagationContext;
    use crate::propagation::Propagator;
    use crate::propagation::PropagatorConstructor;
    use crate::propagation::PropagatorConstructorContext;
    use crate::propagation::PropagatorSpec;
    use crate::propagation::RuntimeCheckers;
    use crate::state::PropagatorHandle;

    /// Counts how often it is notified that its predicate is satisfied.
    #[derive(Clone, Debug)]
    struct SatisfiedCounter {
        predicate: Predicate,
        num_notifications: usize,
    }

    impl PropagatorConstructor for SatisfiedCounter {
        type PropagatorImpl = Self;

        fn create(self, mut context: PropagatorConstructorContext) -> PropagatorSpec<Self> {
            let _ = context.register_predicate(self.predicate);

            PropagatorSpec {
                registration: EventsToRegister::empty(),
                checkers: RuntimeCheckers::empty(),
                propagator: self,
            }
        }
    }

    impl Propagator for SatisfiedCounter {
        fn name(&self) -> &str {
            "SatisfiedCounter"
        }

        fn propagate_from_scratch(&self, _context: PropagationContext) -> PropagationStatusCP {
            Ok(())
        }

        fn notify_predicate_id_satisfied(
            &mut self,
            _context: NotificationContext,
            _predicate_id: PredicateId,
        ) -> EnqueueDecision {
            self.num_notifications += 1;
            EnqueueDecision::Skip
        }
    }

    #[test]
    fn predicate_which_is_true_when_watched_is_not_reported_until_it_is_undone() {
        let mut state = State::default();
        let x = state.new_interval_variable(0, 10, None);

        // `[x >= 5]` becomes true at checkpoint 1 and is watched at checkpoint 2.
        state.new_checkpoint();
        let _ = state.post(predicate![x >= 6]).expect("non-empty domain");
        state.propagate_to_fixed_point().expect("no conflict");

        state.new_checkpoint();
        let handle: PropagatorHandle<SatisfiedCounter> = state.add_propagator(SatisfiedCounter {
            predicate: predicate![x >= 5],
            num_notifications: 0,
        });
        state.propagate_to_fixed_point().expect("no conflict");

        // Neither a later update nor a backtrack which keeps `[x >= 5]` true reports it.
        for lower_bound in [7, 8] {
            state.new_checkpoint();
            let _ = state
                .post(predicate![x >= lower_bound])
                .expect("non-empty domain");
            state.propagate_to_fixed_point().expect("no conflict");
            let _ = state.restore_to(2);
        }
        let _ = state.restore_to(1);
        state.new_checkpoint();
        let _ = state.post(predicate![x >= 9]).expect("non-empty domain");
        state.propagate_to_fixed_point().expect("no conflict");

        let counter = state.get_propagator(handle).expect("propagator exists");
        assert_eq!(counter.num_notifications, 0);

        // Once undone, `[x >= 5]` is reported when it becomes true again.
        let _ = state.restore_to(0);
        state.new_checkpoint();
        let _ = state.post(predicate![x >= 5]).expect("non-empty domain");
        state.propagate_to_fixed_point().expect("no conflict");

        let counter = state.get_propagator(handle).expect("propagator exists");
        assert_eq!(counter.num_notifications, 1);
    }

    #[test]
    fn predicate_which_is_queried_before_it_is_watched_is_not_reported_until_it_is_undone() {
        let mut state = State::default();
        let x = state.new_interval_variable(0, 10, None);

        // `[x >= 5]` becomes true at checkpoint 1, and is queried and then watched at checkpoint
        // 3.
        state.new_checkpoint();
        let _ = state.post(predicate![x >= 6]).expect("non-empty domain");
        state.propagate_to_fixed_point().expect("no conflict");
        state.new_checkpoint();
        state.new_checkpoint();

        let predicate_id = state.notification_engine.get_id(predicate![x >= 5]);
        assert!(
            state
                .notification_engine
                .is_predicate_id_satisfied(predicate_id, &state.assignments)
        );
        let handle: PropagatorHandle<SatisfiedCounter> = state.add_propagator(SatisfiedCounter {
            predicate: predicate![x >= 5],
            num_notifications: 0,
        });
        state.propagate_to_fixed_point().expect("no conflict");

        // A backtrack which keeps `[x >= 5]` true does not lead to it being reported.
        let _ = state.restore_to(2);
        state.new_checkpoint();
        let _ = state.post(predicate![x >= 7]).expect("non-empty domain");
        state.propagate_to_fixed_point().expect("no conflict");

        let counter = state.get_propagator(handle).expect("propagator exists");
        assert_eq!(counter.num_notifications, 0);

        // Once undone, `[x >= 5]` is reported when it becomes true again.
        let _ = state.restore_to(0);
        state.new_checkpoint();
        let _ = state.post(predicate![x >= 5]).expect("non-empty domain");
        state.propagate_to_fixed_point().expect("no conflict");

        let counter = state.get_propagator(handle).expect("propagator exists");
        assert_eq!(counter.num_notifications, 1);
    }
}
