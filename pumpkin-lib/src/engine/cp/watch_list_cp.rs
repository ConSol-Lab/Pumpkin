use crate::{basic_types::DomainId, pumpkin_assert_moderate};

use super::PropagatorVarId;

#[derive(Default)]
pub struct WatchListCP {
    watchers: Vec<WatcherCP>, //[i] contains propagator ids of propagators that watch domain changes of the i-th integer variable
}

pub struct Watchers<'a> {
    propagator_var: PropagatorVarId,
    watch_list: &'a mut WatchListCP,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u8)]
pub enum DomainEvent {
    Assign = 1,
    Any = 2,
    LowerBound = 4,
    UpperBound = 8,
}

impl DomainEvent {
    pub const MAX: u8 = DomainEvent::UpperBound as u8;
}

//public functions
impl WatchListCP {
    pub fn grow(&mut self) {
        self.watchers.push(WatcherCP::default());
    }

    pub fn num_integer_variables(&self) -> u32 {
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
            DomainEvent::Any => &watcher.any_watchers,
            DomainEvent::LowerBound => &watcher.lower_bound_watchers,
            DomainEvent::UpperBound => &watcher.upper_bound_watchers,
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
            DomainEvent::Any => &mut watcher.any_watchers,
            DomainEvent::LowerBound => &mut watcher.lower_bound_watchers,
            DomainEvent::UpperBound => &mut watcher.upper_bound_watchers,
            DomainEvent::Assign => &mut watcher.assign_watchers,
        };

        pumpkin_assert_moderate!(
            event_watcher.contains(&self.propagator_var),
            "Duplicate registration for event {:?} on propagator variable {:?}.",
            event,
            self.propagator_var,
        );

        event_watcher.push(self.propagator_var);
    }
}

#[derive(Default)]
struct WatcherCP {
    pub lower_bound_watchers: Vec<PropagatorVarId>,
    pub upper_bound_watchers: Vec<PropagatorVarId>,
    pub any_watchers: Vec<PropagatorVarId>,
    pub assign_watchers: Vec<PropagatorVarId>,
}
