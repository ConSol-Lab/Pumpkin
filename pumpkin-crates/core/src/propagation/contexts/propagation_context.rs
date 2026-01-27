use crate::basic_types::PredicateId;
use crate::basic_types::RefOrOwned;
use crate::engine::Assignments;
use crate::engine::EmptyDomain;
use crate::engine::EmptyDomainConflict;
use crate::engine::TrailedValues;
use crate::engine::notifications::NotificationEngine;
use crate::engine::notifications::PredicateNotifier;
use crate::engine::notifications::Watchers;
use crate::engine::predicates::predicate::Predicate;
use crate::engine::reason::Reason;
use crate::engine::reason::ReasonStore;
use crate::engine::reason::StoredReason;
use crate::engine::variables::Literal;
use crate::proof::InferenceCode;
use crate::propagation::DomainEvents;
use crate::propagation::Domains;
use crate::propagation::HasAssignments;
use crate::propagation::LocalId;
#[cfg(doc)]
use crate::propagation::Propagator;
#[cfg(doc)]
use crate::propagation::PropagatorConstructorContext;
use crate::propagation::PropagatorId;
use crate::propagation::PropagatorVarId;
#[cfg(doc)]
use crate::propagation::ReadDomains;
use crate::pumpkin_assert_simple;
use crate::variables::IntegerVariable;

/// Provided to the propagator when it is notified of a domain event.
///
/// Domains can be read through the implementation of [`ReadDomains`].
///
/// The difference with [`PropagationContext`] is that it is not possible to perform a propagation
/// in the notify callback.
#[derive(Debug)]
pub struct NotificationContext<'a> {
    pub(crate) trailed_values: &'a mut TrailedValues,
    pub(crate) assignments: &'a Assignments,
    pub(crate) predicate_notifier: &'a mut PredicateNotifier,

    predicates_to_watch: RefOrOwned<'a, Vec<PredicateId>>,
    predicates_to_unwatch: RefOrOwned<'a, Vec<PredicateId>>,
    unregister_domain_events: RefOrOwned<'a, Vec<LocalId>>,
}

impl Drop for NotificationContext<'_> {
    fn drop(&mut self) {
        assert!(
            self.predicates_to_watch.is_empty(),
            "losing predicates that should be watched"
        );
        assert!(
            self.predicates_to_unwatch.is_empty(),
            "losing predicates that should be unwatched"
        );
    }
}

impl<'a> NotificationContext<'a> {
    pub(crate) fn new(
        trailed_values: &'a mut TrailedValues,
        assignments: &'a Assignments,
        predicate_notifier: &'a mut PredicateNotifier,
    ) -> Self {
        Self {
            trailed_values,
            assignments,
            predicate_notifier,
            predicates_to_watch: vec![].into(),
            predicates_to_unwatch: vec![].into(),
            unregister_domain_events: vec![].into(),
        }
    }

    /// Returns true if the given predicate ID is assigned to true.
    pub fn is_predicate_id_satisfied(&mut self, predicate_id: PredicateId) -> bool {
        self.predicate_notifier
            .predicate_id_assignments
            .is_satisfied(
                predicate_id,
                self.assignments,
                &mut self.predicate_notifier.predicate_to_id,
            )
    }

    /// Get the current domains.
    pub fn domains(&mut self) -> Domains<'_> {
        Domains::new(self.assignments, self.trailed_values)
    }

    /// Start watching the given predicate.
    pub fn watch_predicate(&mut self, predicate: Predicate) -> PredicateId {
        let predicate_id = self.predicate_notifier.predicate_to_id.get_id(predicate);
        self.predicates_to_watch.push(predicate_id);
        predicate_id
    }

    /// Stop watching the given predicate.
    pub fn unwatch_predicate(&mut self, predicate_id: PredicateId) {
        self.predicates_to_unwatch.push(predicate_id);
    }

    /// Unregister for all events
    pub fn unregister_domain_event(&mut self, local_id: LocalId) {
        self.unregister_domain_events.push(local_id);
    }

    pub fn reborrow(&mut self) -> NotificationContext<'_> {
        NotificationContext {
            trailed_values: self.trailed_values,
            assignments: self.assignments,
            predicate_notifier: self.predicate_notifier,
            predicates_to_watch: self.predicates_to_watch.reborrow(),
            predicates_to_unwatch: self.predicates_to_unwatch.reborrow(),
            unregister_domain_events: self.unregister_domain_events.reborrow(),
        }
    }

    pub(crate) fn take_watch_changes(mut self) -> WatchChanges {
        WatchChanges {
            start_watching_predicates: std::mem::take(&mut self.predicates_to_watch),
            stop_watching_predicates: std::mem::take(&mut self.predicates_to_unwatch),
            unregister_domain_events: std::mem::take(&mut self.unregister_domain_events),
        }
    }
}

pub(crate) struct WatchChanges {
    pub(crate) start_watching_predicates: Vec<PredicateId>,
    pub(crate) stop_watching_predicates: Vec<PredicateId>,
    pub(crate) unregister_domain_events: Vec<LocalId>,
}

impl Drop for WatchChanges {
    fn drop(&mut self) {
        assert!(
            self.start_watching_predicates.is_empty(),
            "losing predicates that should be watched"
        );
        assert!(
            self.stop_watching_predicates.is_empty(),
            "losing predicates that should be unwatched"
        );
        assert!(
            self.unregister_domain_events.is_empty(),
            "unregister for domain events"
        );
    }
}

impl<'a> HasAssignments for NotificationContext<'a> {
    fn assignments(&self) -> &Assignments {
        self.assignments
    }

    fn trailed_values(&self) -> &TrailedValues {
        self.trailed_values
    }

    fn trailed_values_mut(&mut self) -> &mut TrailedValues {
        self.trailed_values
    }
}

/// Provides information about the state of the solver to a propagator.
///
/// Domains can be read through the implementation of [`ReadDomains`], and changes to the state can
/// be made via [`Self::post`].
#[derive(Debug)]
pub struct PropagationContext<'a> {
    pub(crate) trailed_values: &'a mut TrailedValues,
    pub(crate) assignments: &'a mut Assignments,
    pub(crate) reason_store: &'a mut ReasonStore,
    pub(crate) propagator_id: PropagatorId,
    pub(crate) notification_engine: &'a mut NotificationEngine,
    reification_literal: Option<Literal>,
}

impl<'a> HasAssignments for PropagationContext<'a> {
    fn assignments(&self) -> &Assignments {
        self.assignments
    }

    fn trailed_values(&self) -> &TrailedValues {
        self.trailed_values
    }

    fn trailed_values_mut(&mut self) -> &mut TrailedValues {
        self.trailed_values
    }
}

impl<'a> PropagationContext<'a> {
    pub(crate) fn new(
        trailed_values: &'a mut TrailedValues,
        assignments: &'a mut Assignments,
        reason_store: &'a mut ReasonStore,
        notification_engine: &'a mut NotificationEngine,
        propagator_id: PropagatorId,
    ) -> Self {
        PropagationContext {
            trailed_values,
            assignments,
            reason_store,
            propagator_id,
            notification_engine,
            reification_literal: None,
        }
    }

    /// Register the propagator to be enqueued when the provided [`Predicate`] becomes true.
    ///
    /// Returns the [`PredicateId`] assigned to the provided predicate, which will be provided
    /// to [`Propagator::notify_predicate_id_satisfied`].
    pub fn register_predicate(&mut self, predicate: Predicate) -> PredicateId {
        self.notification_engine.watch_predicate(
            predicate,
            self.propagator_id,
            self.trailed_values,
            self.assignments,
        )
    }

    /// Subscribes the propagator to the given [`DomainEvents`].
    ///
    /// See [`PropagatorConstructorContext::register`] for more information.
    pub fn register_domain_event(
        &mut self,
        var: impl IntegerVariable,
        domain_events: DomainEvents,
        local_id: LocalId,
    ) {
        let propagator_var = PropagatorVarId {
            propagator: self.propagator_id,
            variable: local_id,
        };

        let mut watchers = Watchers::new(propagator_var, self.notification_engine);
        var.watch_all(&mut watchers, domain_events.events());
    }

    /// Get the [`Predicate`] for a given [`PredicateId`].
    pub fn get_predicate(&mut self, predicate_id: PredicateId) -> Predicate {
        self.notification_engine.get_predicate(predicate_id)
    }

    /// Get a [`PredicateId`] for the given [`Predicate`].
    ///
    /// If no ID exists, one will be created.
    pub fn get_id(&mut self, predicate: Predicate) -> PredicateId {
        self.notification_engine.get_id(predicate)
    }

    /// Apply a reification literal to all the explanations that are passed to the context.
    pub(crate) fn with_reification(&mut self, reification_literal: Literal) {
        pumpkin_assert_simple!(
            self.reification_literal.is_none(),
            "cannot reify an already reified propagation context"
        );

        self.reification_literal = Some(reification_literal);
    }

    /// Get the current domain information.
    pub fn domains(&mut self) -> Domains<'_> {
        Domains::new(self.assignments, self.trailed_values)
    }

    pub(crate) fn get_checkpoint(&self) -> usize {
        self.assignments.get_checkpoint()
    }

    /// Returns whether the [`Predicate`] corresponding to the provided [`PredicateId`] is
    /// satisfied.
    pub(crate) fn is_predicate_id_falsified(&mut self, predicate_id: PredicateId) -> bool {
        self.notification_engine
            .is_predicate_id_falsified(predicate_id, self.assignments)
    }

    /// Returns whether the [`Predicate`] corresponding to the provided [`PredicateId`] is
    /// satisfied.
    pub(crate) fn is_predicate_id_satisfied(&mut self, predicate_id: PredicateId) -> bool {
        self.notification_engine
            .is_predicate_id_satisfied(predicate_id, self.assignments)
    }

    /// Returns the number of [`PredicateId`]s.
    pub(crate) fn num_predicate_ids(&self) -> usize {
        self.notification_engine.num_predicate_ids()
    }

    pub fn reborrow(&mut self) -> PropagationContext<'_> {
        PropagationContext {
            trailed_values: self.trailed_values,
            assignments: self.assignments,
            reason_store: self.reason_store,
            propagator_id: self.propagator_id,
            notification_engine: self.notification_engine,
            reification_literal: self.reification_literal,
        }
    }
}

impl PropagationContext<'_> {
    /// Assign the truth-value of the given [`Predicate`] to `true` in the current partial
    /// assignment.
    ///
    /// If the truth-value is already `true`, then this is a no-op. Alternatively, if the
    /// truth-value is `false`, then a conflict is triggered and the [`EmptyDomain`] error is
    /// returned. At that point, no-more propagation should happen.
    pub fn post(
        &mut self,
        predicate: Predicate,
        reason: impl Into<Reason>,
        inference_code: &InferenceCode,
    ) -> Result<(), EmptyDomainConflict> {
        let slot = self.reason_store.new_slot();

        let modification_result = self.assignments.post_predicate(
            predicate,
            Some((slot.reason_ref(), inference_code.clone())),
            self.notification_engine,
        );

        match modification_result {
            Ok(false) => Ok(()),
            Ok(true) => {
                let _ = slot.populate(
                    self.propagator_id,
                    build_reason(reason, self.reification_literal),
                );
                Ok(())
            }
            Err(EmptyDomain) => {
                let _ = slot.populate(
                    self.propagator_id,
                    build_reason(reason, self.reification_literal),
                );
                let (trigger_predicate, trigger_reason, trigger_inference_code) =
                    self.assignments.remove_last_trail_element();

                Err(EmptyDomainConflict {
                    trigger_predicate,
                    trigger_reason,
                    trigger_inference_code,
                })
            }
        }
    }
}

fn build_reason(reason: impl Into<Reason>, reification_literal: Option<Literal>) -> StoredReason {
    match reason.into() {
        Reason::Eager(mut conjunction) => {
            conjunction.extend(
                reification_literal
                    .iter()
                    .map(|lit| lit.get_true_predicate()),
            );
            StoredReason::Eager(conjunction)
        }
        Reason::DynamicLazy(code) => StoredReason::DynamicLazy(code),
    }
}
