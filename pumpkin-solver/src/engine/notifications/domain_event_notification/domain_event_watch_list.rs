use std::fmt::Display;

use enumset::EnumSet;
use enumset::EnumSetType;

use crate::containers::KeyedVec;
use crate::engine::notifications::NotificationEngine;
use crate::engine::propagation::PropagatorVarId;
use crate::engine::variables::DomainId;
use crate::engine::Assignments;
use crate::engine::TrailedValues;
use crate::predicates::Predicate;

#[derive(Default, Debug)]
pub(crate) struct WatchListDomainEvents {
    /// Contains propagator ids of propagators that watch domain changes of the i-th integer
    /// variable
    pub(crate) watchers: KeyedVec<DomainId, WatcherDomainEvents>,
    pub(crate) is_watching_anything: bool,
    pub(crate) is_watching_any_backtrack_events: bool,
}

/// Used to register a propagator for notifications about events to a particular variable
#[derive(Debug)]
pub struct Watchers<'a> {
    propagator_var: PropagatorVarId,
    notification_engine: &'a mut NotificationEngine,
    trailed_values: &'a mut TrailedValues,
    assignments: &'a Assignments,
}

/// A description of the kinds of events that can happen on a domain variable.
#[derive(Debug, EnumSetType, Hash)]
pub enum DomainEvent {
    /// Event where an (integer) variable domain collapses to a single value.
    Assign,
    /// Event where an (integer) variable domain tightens the lower bound.
    LowerBound,
    /// Event where an (integer) variable domain tightens the upper bound.
    UpperBound,
    /// Event where an (integer) variable domain removes an inner value within the domain.
    /// N.B. this DomainEvent should not be subscribed to by itself!
    #[doc(hidden)]
    Removal,
}

impl Display for DomainEvent {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DomainEvent::Assign => write!(f, "[Event:Assign]"),
            DomainEvent::LowerBound => write!(f, "[Event:LB]"),
            DomainEvent::UpperBound => write!(f, "[Event:UB]"),
            DomainEvent::Removal => write!(f, "[Event:Remove]"),
        }
    }
}

// public functions
impl WatchListDomainEvents {
    pub(crate) fn grow(&mut self) {
        let _ = self.watchers.push(WatcherDomainEvents::default());
    }

    pub(crate) fn is_watching_any_backtrack_events(&self) -> bool {
        self.is_watching_any_backtrack_events
    }

    pub(crate) fn get_affected_propagators(
        &self,
        event: DomainEvent,
        domain: DomainId,
    ) -> &[PropagatorVarId] {
        let watcher = &self.watchers[domain];

        match event {
            DomainEvent::Assign => &watcher.forward_watcher.assign_watchers,
            DomainEvent::LowerBound => &watcher.forward_watcher.lower_bound_watchers,
            DomainEvent::UpperBound => &watcher.forward_watcher.upper_bound_watchers,
            DomainEvent::Removal => &watcher.forward_watcher.removal_watchers,
        }
    }

    pub(crate) fn get_backtrack_affected_propagators(
        &self,
        event: DomainEvent,
        domain: DomainId,
    ) -> &[PropagatorVarId] {
        let watcher = &self.watchers[domain];

        match event {
            DomainEvent::Assign => &watcher.backtrack_watcher.assign_watchers,
            DomainEvent::LowerBound => &watcher.backtrack_watcher.lower_bound_watchers,
            DomainEvent::UpperBound => &watcher.backtrack_watcher.upper_bound_watchers,
            DomainEvent::Removal => &watcher.backtrack_watcher.removal_watchers,
        }
    }
}

impl<'a> Watchers<'a> {
    pub(crate) fn new(
        propagator_var: PropagatorVarId,
        notification_engine: &'a mut NotificationEngine,
        trailed_values: &'a mut TrailedValues,
        assignments: &'a Assignments,
    ) -> Self {
        Watchers {
            propagator_var,
            notification_engine,
            trailed_values,
            assignments,
        }
    }

    pub(crate) fn watch_predicate(&mut self, predicate: Predicate) {
        self.notification_engine.watch_predicate(
            predicate,
            self.propagator_var,
            self.trailed_values,
            self.assignments,
        );
    }

    pub(crate) fn watch_all(&mut self, domain: DomainId, events: EnumSet<DomainEvent>) {
        self.notification_engine
            .watch_all(domain, events, self.propagator_var);
    }

    pub(crate) fn watch_all_backtrack(&mut self, domain: DomainId, events: EnumSet<DomainEvent>) {
        self.notification_engine
            .watch_all_backtrack(domain, events, self.propagator_var);
    }
}

#[derive(Default, Debug)]
pub(crate) struct WatcherDomainEvents {
    // FIXME measure performance of these vectors, they are treated as sets
    pub(crate) forward_watcher: Watcher,
    pub(crate) backtrack_watcher: Watcher,
}

#[derive(Debug, Default)]
pub(crate) struct Watcher {
    pub(crate) lower_bound_watchers: Vec<PropagatorVarId>,
    pub(crate) upper_bound_watchers: Vec<PropagatorVarId>,
    pub(crate) assign_watchers: Vec<PropagatorVarId>,
    pub(crate) removal_watchers: Vec<PropagatorVarId>,
}
