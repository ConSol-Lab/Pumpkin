use enumset::EnumSet;

use super::DomainEventWatchList;
use super::IntDomainEvent;
use super::WatchList;
use crate::engine::propagation::PropagatorVarId;
use crate::engine::variables::DomainId;

/// Used to register a propagator for notifications about events to a particular variable
#[derive(Debug)]
pub struct PropagatorWatchers<'a> {
    propagator_var: PropagatorVarId,
    watch_list: &'a mut DomainEventWatchList<DomainId, PropagatorVarId>,
}

impl<'a> PropagatorWatchers<'a> {
    pub(crate) fn new(
        propagator_var: PropagatorVarId,
        watch_list: &'a mut DomainEventWatchList<DomainId, PropagatorVarId>,
    ) -> Self {
        PropagatorWatchers {
            propagator_var,
            watch_list,
        }
    }

    pub(crate) fn watch_all(&mut self, domain: DomainId, events: EnumSet<IntDomainEvent>) {
        self.watch_list.watches();
        self.watch_list.watch(self.propagator_var, domain, events)
    }

    pub(crate) fn watch_all_backtrack(
        &mut self,
        domain: DomainId,
        events: EnumSet<IntDomainEvent>,
    ) {
        self.watch_list.watches_backtrack();
        self.watch_list
            .watch_backtrack(self.propagator_var, domain, events)
    }
}
