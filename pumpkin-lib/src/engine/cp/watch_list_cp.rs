use std::fmt::Display;

use enumset::EnumSet;
use enumset::EnumSetType;

use crate::basic_types::KeyedVec;
use crate::engine::propagation::PropagatorVarId;
use crate::engine::variables::DomainId;

#[derive(Default, Debug)]
pub(crate) struct WatchListCP {
    watchers: KeyedVec<DomainId, WatcherCP>, /* contains propagator ids of propagators that
                                              * watch domain changes of the i-th integer
                                              * variable */
    is_watching_anything: bool,
}

#[derive(Debug)]
pub struct Watchers<'a> {
    propagator_var: PropagatorVarId,
    watch_list: &'a mut WatchListCP,
}

/// A description of the kinds of events that can happen on a domain variable.
#[derive(Debug, EnumSetType, Hash)]
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

impl Display for IntDomainEvent {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IntDomainEvent::Assign => write!(f, "[Event:Assign]"),
            IntDomainEvent::LowerBound => write!(f, "[Event:LB]"),
            IntDomainEvent::UpperBound => write!(f, "[Event:UB]"),
            IntDomainEvent::Removal => write!(f, "[Event:Remove]"),
        }
    }
}

// public functions
impl WatchListCP {
    pub(crate) fn grow(&mut self) {
        self.watchers.push(WatcherCP::default());
    }

    pub(crate) fn is_watching_anything(&self) -> bool {
        self.is_watching_anything
    }

    pub(crate) fn get_affected_propagators(
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

    pub(crate) fn watch_all(&mut self, domain: DomainId, events: EnumSet<IntDomainEvent>) {
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

#[derive(Default, Debug)]
struct WatcherCP {
    // FIXME measure performance of these vectors, they are treated as sets
    lower_bound_watchers: Vec<PropagatorVarId>,
    upper_bound_watchers: Vec<PropagatorVarId>,
    assign_watchers: Vec<PropagatorVarId>,
    removal_watchers: Vec<PropagatorVarId>,
}
