use crate::basic_types::DomainId;
use enumset::{EnumSet, EnumSetType};

use super::PropagatorVarId;

#[derive(Default)]
pub struct WatchListCP {
    watchers: Vec<WatcherCP>, //[i] contains propagator ids of propagators that watch domain changes of the i-th integer variable
}

pub struct Watchers<'a> {
    propagator_var: PropagatorVarId,
    watch_list: &'a mut WatchListCP,
}

/// A description of the kinds of events that can happen on a domain variable.
#[derive(Debug, EnumSetType)]
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

//public functions
impl WatchListCP {
    pub fn grow(&mut self) {
        self.watchers.push(WatcherCP::default());
    }

    pub fn num_domains(&self) -> u32 {
        self.watchers.len() as u32
    }

    pub fn get_affected_propagators(
        &self,
        event: DomainEvent,
        domain: DomainId,
    ) -> &[PropagatorVarId] {
        let watcher = &self.watchers[domain];

        match event {
            DomainEvent::Assign => &watcher.assign_watchers,
            DomainEvent::LowerBound => &watcher.lower_bound_watchers,
            DomainEvent::UpperBound => &watcher.upper_bound_watchers,
            DomainEvent::Removal => &watcher.removal_watchers,
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
        let watcher = &mut self.watch_list.watchers[domain];

        let event_watcher = match event {
            DomainEvent::LowerBound => &mut watcher.lower_bound_watchers,
            DomainEvent::UpperBound => &mut watcher.upper_bound_watchers,
            DomainEvent::Assign => &mut watcher.assign_watchers,
            DomainEvent::Removal => &mut watcher.removal_watchers,
        };

        if !event_watcher.contains(&self.propagator_var) {
            event_watcher.push(self.propagator_var);
        }
    }

    pub fn watch_all(&mut self, domain: DomainId, events: EnumSet<DomainEvent>) {
        let watcher = &mut self.watch_list.watchers[domain];

        for event in events {
            let event_watcher = match event {
                DomainEvent::LowerBound => &mut watcher.lower_bound_watchers,
                DomainEvent::UpperBound => &mut watcher.upper_bound_watchers,
                DomainEvent::Assign => &mut watcher.assign_watchers,
                DomainEvent::Removal => &mut watcher.removal_watchers,
            };

            if !event_watcher.contains(&self.propagator_var) {
                event_watcher.push(self.propagator_var);
            }
        }
    }
}

#[derive(Default)]
struct WatcherCP {
    // FIXME measure performance of these vectors, they are treated as sets
    pub lower_bound_watchers: Vec<PropagatorVarId>,
    pub upper_bound_watchers: Vec<PropagatorVarId>,
    pub assign_watchers: Vec<PropagatorVarId>,
    pub removal_watchers: Vec<PropagatorVarId>,
}
