pub(crate) mod domain_event_notification;
pub(crate) mod predicate_notification;

pub(crate) use domain_event_notification::domain_events::DomainEvents;
use domain_event_notification::DomainEvent;
pub(crate) use domain_event_notification::EventSink;
pub(crate) use domain_event_notification::WatchListDomainEvents;
pub(crate) use domain_event_notification::Watchers;
pub(crate) use predicate_notification::PredicateNotifier;

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

/// Process the stored domain events that happens as a result of decision/propagation predicates
/// to the trail. Propagators are notified and enqueued if needed about the domain events.
pub(crate) fn notify_propagators_about_domain_events(
    predicate_notifier: &mut PredicateNotifier,
    assignments: &mut Assignments,
    trailed_values: &mut TrailedValues,
    propagators: &mut PropagatorStore,
    propagator_queue: &mut PropagatorQueue,
    watch_list_domain_events: &WatchListDomainEvents,
    last_notified_cp_trail_index: &mut usize,
) {
    // Collect so that we can pass the assignments to the methods within the loop
    for (event, domain) in assignments.drain_domain_events().collect::<Vec<_>>() {
        // First we notify the predicate_notifier that a domain has been updated
        predicate_notifier.on_update(trailed_values, assignments, event, domain);
        // Special case: the nogood propagator is notified about each event.
        notify_nogood_propagator(
            event,
            domain,
            propagators,
            propagator_queue,
            assignments,
            trailed_values,
        );
        // Now notify other propagators subscribed to this event.
        for propagator_var in watch_list_domain_events.get_affected_propagators(event, domain) {
            let propagator_id = propagator_var.propagator;
            let local_id = propagator_var.variable;
            notify_propagator(
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
    notify_predicate_id_satisfied(predicate_notifier, propagators);
    notify_predicate_id_falsified(predicate_notifier, propagators);

    *last_notified_cp_trail_index = assignments.num_trail_entries();
}

/// Notifies propagators that certain [`Predicate`]s have been falsified.
///
/// Currently, no propagators are informed of this information.
fn notify_predicate_id_falsified(
    _predicate_notifier: &mut PredicateNotifier,
    _propagators: &mut PropagatorStore,
) {
    // At the moment this does nothing
}

/// Notifies propagators that certain [`Predicate`]s have been satisfied.
///
/// Currently, only the [`NogoodPropagator`] is notified.
fn notify_predicate_id_satisfied(
    predicate_notifier: &mut PredicateNotifier,
    propagators: &mut PropagatorStore,
) {
    for predicate_id in predicate_notifier
        .drain_satisfied_predicates()
        .collect::<Vec<_>>()
    {
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
    notify_propagator(
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
        propagator_queue.enqueue_propagator(propagator_id, propagators[propagator_id].priority());
    }
}
