use std::fmt::Display;

use enumset::EnumSet;
use enumset::EnumSetType;

use crate::containers::KeyedVec;
use crate::engine::propagation::PropagatorVarId;
use crate::engine::variables::DomainId;

#[derive(Default, Debug)]
pub(crate) struct WatchListDomainEvents {
    /// Contains propagator ids of propagators that watch domain changes of the i-th integer
    /// variable
    watchers: KeyedVec<DomainId, WatcherDomainEvents>,
    is_watching_anything: bool,
    is_watching_any_backtrack_events: bool,
}

/// Used to register a propagator for notifications about events to a particular variable
#[derive(Debug)]
pub struct Watchers<'a> {
    propagator_var: PropagatorVarId,
    watch_list: &'a mut WatchListDomainEvents,
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
        watch_list: &'a mut WatchListDomainEvents,
    ) -> Self {
        Watchers {
            propagator_var,
            watch_list,
        }
    }

    pub(crate) fn watch_all(&mut self, domain: DomainId, events: EnumSet<DomainEvent>) {
        self.watch_list.is_watching_anything = true;
        let watcher = &mut self.watch_list.watchers[domain];

        for event in events {
            let event_watcher = match event {
                DomainEvent::LowerBound => &mut watcher.forward_watcher.lower_bound_watchers,
                DomainEvent::UpperBound => &mut watcher.forward_watcher.upper_bound_watchers,
                DomainEvent::Assign => &mut watcher.forward_watcher.assign_watchers,
                DomainEvent::Removal => &mut watcher.forward_watcher.removal_watchers,
            };

            if !event_watcher.contains(&self.propagator_var) {
                event_watcher.push(self.propagator_var);
            }
        }
    }

    pub(crate) fn watch_all_backtrack(&mut self, domain: DomainId, events: EnumSet<DomainEvent>) {
        self.watch_list.is_watching_any_backtrack_events = true;
        let watcher = &mut self.watch_list.watchers[domain];

        for event in events {
            let backtrack_event_watchers = match event {
                DomainEvent::Assign => &mut watcher.backtrack_watcher.assign_watchers,
                DomainEvent::LowerBound => &mut watcher.backtrack_watcher.lower_bound_watchers,
                DomainEvent::UpperBound => &mut watcher.backtrack_watcher.upper_bound_watchers,
                DomainEvent::Removal => &mut watcher.backtrack_watcher.removal_watchers,
            };

            if !backtrack_event_watchers.contains(&self.propagator_var) {
                backtrack_event_watchers.push(self.propagator_var)
            }
        }
    }
}

#[derive(Default, Debug)]
struct WatcherDomainEvents {
    // FIXME measure performance of these vectors, they are treated as sets
    forward_watcher: Watcher,
    backtrack_watcher: Watcher,
}

#[derive(Debug, Default)]
struct Watcher {
    lower_bound_watchers: Vec<PropagatorVarId>,
    upper_bound_watchers: Vec<PropagatorVarId>,
    assign_watchers: Vec<PropagatorVarId>,
    removal_watchers: Vec<PropagatorVarId>,
}
