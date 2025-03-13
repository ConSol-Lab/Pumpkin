use enumset::EnumSet;

use super::PredicateDomainEvent;
use super::WatchList;
use crate::containers::KeyedVec;
use crate::containers::StorageKey;

/// A generic watch list for watching forward and backward events (e.g. assignments to variables
/// and backtracking from those assignments)
#[derive(Debug)]
pub(crate) struct PredicateEventWatchList<WatchingType: StorageKey, StoredType> {
    watchers: KeyedVec<WatchingType, PredicateEventWatcher<Vec<StoredType>>>,
    is_watching_anything: bool,
}

impl<WatchingType: StorageKey, StoredType> Default
    for PredicateEventWatchList<WatchingType, StoredType>
{
    fn default() -> Self {
        Self {
            watchers: KeyedVec::default(),
            is_watching_anything: false,
        }
    }
}

impl<WatchingType: StorageKey, StoredType: PartialEq + Clone>
    WatchList<WatchingType, StoredType, PredicateDomainEvent>
    for PredicateEventWatchList<WatchingType, StoredType>
{
    fn grow(&mut self) {
        let _ = self.watchers.push(PredicateEventWatcher::default());
    }

    fn is_watching_anything(&self) -> bool {
        self.is_watching_anything
    }

    fn watches(&mut self) {
        self.is_watching_anything = true;
    }

    fn get_affected(&mut self, event: PredicateDomainEvent, domain: WatchingType) -> &[StoredType] {
        if domain.index() >= self.watchers.len() {
            return &[];
        }

        let watcher_predicate = &self.watchers[domain];

        match event {
            PredicateDomainEvent::AssignTrue => &watcher_predicate.assigned_true_watchers,
            PredicateDomainEvent::AssignFalse => &watcher_predicate.assigned_false_watchers,
        }
    }

    fn watch(
        &mut self,
        watcher: StoredType,
        watches: WatchingType,
        events: EnumSet<PredicateDomainEvent>,
    ) {
        while watches.index() >= self.watchers.len() {
            let _ = self.watchers.push(PredicateEventWatcher::default());
        }
        let watcher_predicate = &mut self.watchers[watches];

        for event in events {
            let event_watcher = match event {
                PredicateDomainEvent::AssignTrue => &mut watcher_predicate.assigned_true_watchers,
                PredicateDomainEvent::AssignFalse => &mut watcher_predicate.assigned_false_watchers,
            };

            if !event_watcher.contains(&watcher) {
                event_watcher.push(watcher.clone());
            }
        }
    }
}

/// Tracks watchers for the different kinds of [`PredicateDomainEvent`].
#[derive(Debug, Default)]
pub(super) struct PredicateEventWatcher<StoredType> {
    assigned_true_watchers: StoredType,
    assigned_false_watchers: StoredType,
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub(crate) enum PredicateWatcher {
    Brancher,
    NogoodPropagator,
}
