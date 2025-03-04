use crate::{basic_types::PredicateId, engine::propagation::PropagatorVarId, variables::DomainId};

use super::{
    DomainEventWatchList, IntDomainEvent, PredicateEventWatchList, PredicateWatcher, WatchList,
};

#[derive(Debug, Default)]
pub(crate) struct WatchListManager {
    pub(crate) watch_list_cp: DomainEventWatchList<DomainId, PropagatorVarId>,
    watch_list_predicate: PredicateEventWatchList<PredicateId, PredicateWatcher>,
}

impl WatchListManager {
    pub(crate) fn get_affected_cp(
        &self,
        event: IntDomainEvent,
        domain: DomainId,
    ) -> &[PropagatorVarId] {
        self.watch_list_cp.get_affected(event, domain)
    }

    pub(crate) fn get_affected_backtrack_cp(
        &self,
        event: IntDomainEvent,
        domain: DomainId,
    ) -> &[PropagatorVarId] {
        self.watch_list_cp.get_affected(event, domain)
    }

    pub(crate) fn grow_cp(&mut self) {
        self.watch_list_cp.grow()
    }

    pub(crate) fn is_watching_anything_backtrack_cp(&self) -> bool {
        self.watch_list_cp.is_watching_anything_backtrack()
    }
}
