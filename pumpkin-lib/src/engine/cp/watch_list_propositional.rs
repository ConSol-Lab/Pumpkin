use enumset::EnumSet;
use enumset::EnumSetType;

use crate::basic_types::KeyedVec;
use crate::engine::propagation::PropagatorVarId;
use crate::engine::variables::Literal;

#[derive(Debug)]
pub(crate) struct WatchListPropositional {
    watchers: KeyedVec<Literal, WatcherPropositional>, /* contains propagator ids of propagators
                                                        * that watch domain changes of the i-th
                                                        * integer variable */
    is_watching_anything: bool,
}

impl Default for WatchListPropositional {
    fn default() -> Self {
        Self {
            watchers: KeyedVec::new(vec![WatcherPropositional::default()]),
            is_watching_anything: false,
        }
    }
}

#[derive(Debug)]
pub(crate) struct WatchersPropositional<'a> {
    propagator_var: PropagatorVarId,
    watch_list: &'a mut WatchListPropositional,
}

#[derive(Debug, EnumSetType)]
pub enum BooleanDomainEvent {
    AssignedTrue,
    AssignedFalse,
}

impl BooleanDomainEvent {
    pub(crate) fn get_iterator(
        literal: Literal,
    ) -> impl Iterator<Item = (BooleanDomainEvent, Literal)> {
        [
            (BooleanDomainEvent::AssignedTrue, literal),
            (BooleanDomainEvent::AssignedFalse, !literal),
        ]
        .into_iter()
    }
}

// public functions
impl WatchListPropositional {
    pub(crate) fn grow(&mut self) {
        let _ = self.watchers.push(WatcherPropositional::default());
        let _ = self.watchers.push(WatcherPropositional::default());
    }

    pub(crate) fn is_watching_anything(&self) -> bool {
        self.is_watching_anything
    }

    pub(crate) fn get_affected_propagators(
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

    pub(crate) fn watch_all(&mut self, domain: Literal, events: EnumSet<BooleanDomainEvent>) {
        self.watch_list.is_watching_anything = true;
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

#[derive(Default, Debug)]
struct WatcherPropositional {
    assigned_true_watchers: Vec<PropagatorVarId>,
    assigned_false_watchers: Vec<PropagatorVarId>,
}
