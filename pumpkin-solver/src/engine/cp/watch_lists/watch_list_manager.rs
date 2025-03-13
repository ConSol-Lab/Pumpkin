use super::DomainEventWatchList;
use super::IntDomainEvent;
use super::PredicateEventWatchList;
use super::PredicateWatcher;
use super::WatchList;
use crate::basic_types::PredicateId;
use crate::engine::propagation::PropagatorVarId;
use crate::variables::DomainId;

#[derive(Debug, Default)]
pub struct WatchListManager {
    pub(crate) watch_list_cp: DomainEventWatchList<DomainId, PropagatorVarId>,
    pub(crate) watch_list_predicate: PredicateEventWatchList<PredicateId, PredicateWatcher>,
}

impl WatchListManager {
    pub(crate) fn get_affected_cp(
        &mut self,
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
        self.watch_list_cp.get_affected_backtrack(event, domain)
    }

    pub(crate) fn grow_cp(&mut self) {
        self.watch_list_cp.grow()
    }

    pub(crate) fn is_watching_anything_backtrack_cp(&self) -> bool {
        self.watch_list_cp.is_watching_anything_backtrack()
    }
}
