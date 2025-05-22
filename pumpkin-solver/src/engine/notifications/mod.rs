pub(crate) mod domain_event_notification;
pub(crate) mod predicate_notification;

pub(crate) use domain_event_notification::domain_events::DomainEvents;
use domain_event_notification::DomainEvent;
pub(crate) use domain_event_notification::EventSink;
pub(crate) use domain_event_notification::WatchListDomainEvents;
pub(crate) use domain_event_notification::Watchers;
pub(crate) use predicate_notification::PredicateNotifier;

use super::propagation::PropagationContext;
use crate::engine::propagation::contexts::PropagationContextWithTrailedValues;
use crate::engine::propagation::store::PropagatorStore;
use crate::engine::propagation::EnqueueDecision;
use crate::engine::propagation::LocalId;
use crate::engine::propagation::PropagatorId;
use crate::engine::Assignments;
use crate::engine::ConstraintSatisfactionSolver;
use crate::engine::PropagatorQueue;
use crate::engine::TrailedValues;
use crate::pumpkin_assert_moderate;
use crate::variables::DomainId;

#[derive(Debug)]
pub(crate) struct NotificationEngine {
    /// Responsible for the notification of predicates becoming either falsified or satisfied.
    pub(crate) predicate_notifier: PredicateNotifier,
    /// Contains events that need to be processed to notify propagators of event occurrences.
    /// Used as a helper storage vector to avoid reallocation, and to take away ownership from the
    /// events in assignments.
    pub(crate) event_drain: Vec<(DomainEvent, DomainId)>,
    /// Contains events that need to be processed to notify propagators of backtrack
    /// [`DomainEvent`] occurrences (i.e. [`IntDomainEvent`]s being undone).
    pub(crate) backtrack_event_drain: Vec<(DomainEvent, DomainId)>,
    /// The trail index for which the last notification took place.
    pub(crate) last_notified_cp_trail_index: usize,
    /// Contains information on which propagator to notify upon
    /// integer events, e.g., lower or upper bound change of a variable.
    pub(crate) watch_list_domain_events: WatchListDomainEvents,
}

impl Default for NotificationEngine {
    fn default() -> Self {
        let mut watch_list_domain_events: WatchListDomainEvents = Default::default();
        // We grow for the dummy predicate
        watch_list_domain_events.grow();
        Self {
            watch_list_domain_events,
            predicate_notifier: Default::default(),
            event_drain: Default::default(),
            backtrack_event_drain: Default::default(),
            last_notified_cp_trail_index: 0,
        }
    }
}

impl NotificationEngine {
    pub(crate) fn grow(&mut self) {
        self.watch_list_domain_events.grow()
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
        for (event, domain) in assignments.drain_domain_events().collect::<Vec<_>>() {
            // First we notify the predicate_notifier that a domain has been updated
            self.predicate_notifier
                .on_update(trailed_values, assignments, event, domain);
            // Special case: the nogood propagator is notified about each event.
            Self::notify_nogood_propagator(
                event,
                domain,
                propagators,
                propagator_queue,
                assignments,
                trailed_values,
            );
            // Now notify other propagators subscribed to this event.
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

        // Then we notify the propagators that a predicate has been satisfied
        self.notify_predicate_id_satisfied(propagators);
        self.notify_predicate_id_falsified(propagators);

        self.last_notified_cp_trail_index = assignments.num_trail_entries();
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
            self.backtrack_event_drain
                .extend(assignments.drain_backtrack_domain_events());

            if self.backtrack_event_drain.is_empty() {
                return false;
            }

            for (event, domain) in self.backtrack_event_drain.drain(..) {
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
        Self::notify_propagator(
            nogood_propagator_id,
            local_id,
            event,
            propagators,
            propagator_queue,
            assignments,
            trailed_values,
        );
    }

    fn notify_propagator(
        propagator_id: PropagatorId,
        local_id: LocalId,
        event: DomainEvent,
        propagators: &mut PropagatorStore,
        propagator_queue: &mut PropagatorQueue,
        assignments: &mut Assignments,
        trailed_values: &mut TrailedValues,
    ) {
        let context = PropagationContextWithTrailedValues::new(trailed_values, assignments);

        let enqueue_decision = propagators[propagator_id].notify(context, local_id, event.into());

        if enqueue_decision == EnqueueDecision::Enqueue {
            propagator_queue
                .enqueue_propagator(propagator_id, propagators[propagator_id].priority());
        }
    }
}
