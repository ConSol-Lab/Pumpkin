use crate::basic_types::Literal;
use enumset::{EnumSet, EnumSetType};

use super::PropagatorVarId;

pub struct WatchListPropositional {
    watchers: Vec<WatcherPropositional>, //[i] contains propagator ids of propagators that watch domain changes of the i-th integer variable
}

impl Default for WatchListPropositional {
    fn default() -> Self {
        Self {
            watchers: vec![WatcherPropositional::default()],
        }
    }
}

pub struct WatchersPropositional<'a> {
    propagator_var: PropagatorVarId,
    watch_list: &'a mut WatchListPropositional,
}

#[derive(Debug, EnumSetType)]
pub enum BooleanDomainEvent {
    AssignedTrue,
    AssignedFalse,
}

impl BooleanDomainEvent {
    pub fn get_iterator(literal: Literal) -> impl Iterator<Item = (BooleanDomainEvent, Literal)> {
        [
            (BooleanDomainEvent::AssignedTrue, literal),
            (BooleanDomainEvent::AssignedFalse, !literal),
        ]
        .into_iter()
    }
}

//public functions
impl WatchListPropositional {
    pub fn grow(&mut self) {
        self.watchers.push(WatcherPropositional::default());
        self.watchers.push(WatcherPropositional::default());
    }

    pub fn num_domains(&self) -> u32 {
        self.watchers.len() as u32
    }

    pub fn get_affected_propagators(
        &self,
        event: BooleanDomainEvent,
        domain: Literal,
    ) -> &[PropagatorVarId] {
        let watcher = &self.watchers[domain];

        match event {
            BooleanDomainEvent::AssignedTrue => &watcher.assigned_true_watchers,
            BooleanDomainEvent::AssignedFalse => &watcher.assigned_false_watchers,
        }
    }
}

impl<'a> WatchersPropositional<'a> {
    pub(crate) fn new(
        propagator_var: PropagatorVarId,
        watch_list: &'a mut WatchListPropositional,
    ) -> Self {
        WatchersPropositional {
            propagator_var,
            watch_list,
        }
    }

    pub fn watch_all(&mut self, domain: Literal, events: EnumSet<BooleanDomainEvent>) {
        let watcher = &mut self.watch_list.watchers[domain];

        for event in events {
            let event_watcher = match event {
                BooleanDomainEvent::AssignedTrue => &mut watcher.assigned_true_watchers,
                BooleanDomainEvent::AssignedFalse => &mut watcher.assigned_false_watchers,
            };

            if !event_watcher.contains(&self.propagator_var) {
                event_watcher.push(self.propagator_var);
            }
        }
    }
}

#[derive(Default)]
struct WatcherPropositional {
    pub assigned_true_watchers: Vec<PropagatorVarId>,
    pub assigned_false_watchers: Vec<PropagatorVarId>,
}
