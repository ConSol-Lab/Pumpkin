use enumset::EnumSet;
use enumset::EnumSetType;

use super::PropagatorVarId;
use crate::basic_types::DomainId;
use crate::basic_types::KeyedVec;

#[derive(Default)]
pub struct WatchListCP {
    watchers: KeyedVec<DomainId, WatcherCP>, //[i] contains propagator ids of propagators that watch domain changes of the i-th integer variable
    is_watching_anything: bool,
}

pub struct Watchers<'a> {
    propagator_var: PropagatorVarId,
    watch_list: &'a mut WatchListCP,
}

/// A description of the kinds of events that can happen on a domain variable.
#[derive(Debug, EnumSetType)]
pub enum IntDomainEvent {
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

    pub fn is_watching_anything(&self) -> bool {
        self.is_watching_anything
    }

    pub fn num_domains(&self) -> u32 {
        self.watchers.len() as u32
    }

    pub fn get_affected_propagators(
        &self,
        event: IntDomainEvent,
        domain: DomainId,
    ) -> &[PropagatorVarId] {
        let watcher = &self.watchers[domain];

        match event {
            IntDomainEvent::Assign => &watcher.assign_watchers,
            IntDomainEvent::LowerBound => &watcher.lower_bound_watchers,
            IntDomainEvent::UpperBound => &watcher.upper_bound_watchers,
            IntDomainEvent::Removal => &watcher.removal_watchers,
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

    pub fn watch(&mut self, domain: DomainId, event: IntDomainEvent) {
        let watcher = &mut self.watch_list.watchers[domain];

        let event_watcher = match event {
            IntDomainEvent::LowerBound => &mut watcher.lower_bound_watchers,
            IntDomainEvent::UpperBound => &mut watcher.upper_bound_watchers,
            IntDomainEvent::Assign => &mut watcher.assign_watchers,
            IntDomainEvent::Removal => &mut watcher.removal_watchers,
        };

        if !event_watcher.contains(&self.propagator_var) {
            event_watcher.push(self.propagator_var);
        }
    }

    pub fn watch_all(&mut self, domain: DomainId, events: EnumSet<IntDomainEvent>) {
        self.watch_list.is_watching_anything = true;
        let watcher = &mut self.watch_list.watchers[domain];

        for event in events {
            let event_watcher = match event {
                IntDomainEvent::LowerBound => &mut watcher.lower_bound_watchers,
                IntDomainEvent::UpperBound => &mut watcher.upper_bound_watchers,
                IntDomainEvent::Assign => &mut watcher.assign_watchers,
                IntDomainEvent::Removal => &mut watcher.removal_watchers,
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
