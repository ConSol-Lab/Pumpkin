use enumset::EnumSet;

use super::IntDomainEvent;
use super::WatchList;
use crate::containers::KeyedVec;
use crate::containers::StorageKey;

/// A generic watch list for watching forward and backward events (e.g. assignments to variables
/// and backtracking from those assignments)
#[derive(Debug)]
pub(crate) struct DomainEventWatchList<WatchingType: StorageKey, StoredType> {
    watchers: KeyedVec<WatchingType, ForwardBackwardWatcher<Vec<StoredType>>>,
    is_watching_anything: bool,
    backtrack_is_watching_anything: bool,
}

impl<WatchingType: StorageKey, StoredType> Default
    for DomainEventWatchList<WatchingType, StoredType>
{
    fn default() -> Self {
        Self {
            watchers: KeyedVec::default(),
            is_watching_anything: false,
            backtrack_is_watching_anything: false,
        }
    }
}

impl<WatchingType: StorageKey, StoredType: PartialEq + Clone>
    WatchList<WatchingType, StoredType, IntDomainEvent>
    for DomainEventWatchList<WatchingType, StoredType>
{
    fn grow(&mut self) {
        let _ = self.watchers.push(ForwardBackwardWatcher::default());
    }

    fn is_watching_anything(&self) -> bool {
        self.is_watching_anything
    }

    fn watches(&mut self) {
        self.is_watching_anything = true;
    }

    fn get_affected(&self, event: IntDomainEvent, domain: WatchingType) -> &[StoredType] {
        let watcher = &self.watchers[domain];

        match event {
            IntDomainEvent::Assign => &watcher.forward_watcher.assign_watchers,
            IntDomainEvent::LowerBound => &watcher.forward_watcher.lower_bound_watchers,
            IntDomainEvent::UpperBound => &watcher.forward_watcher.upper_bound_watchers,
            IntDomainEvent::Removal => &watcher.forward_watcher.removal_watchers,
        }
    }

    fn is_watching_anything_backtrack(&self) -> bool {
        self.backtrack_is_watching_anything
    }

    fn watches_backtrack(&mut self) {
        self.backtrack_is_watching_anything = true;
    }

    fn get_affected_backtrack(&self, event: IntDomainEvent, domain: WatchingType) -> &[StoredType] {
        let watcher = &self.watchers[domain];

        match event {
            IntDomainEvent::Assign => &watcher.backtrack_watcher.assign_watchers,
            IntDomainEvent::LowerBound => &watcher.backtrack_watcher.lower_bound_watchers,
            IntDomainEvent::UpperBound => &watcher.backtrack_watcher.upper_bound_watchers,
            IntDomainEvent::Removal => &watcher.backtrack_watcher.removal_watchers,
        }
    }

    fn watch(
        &mut self,
        watcher: StoredType,
        watches: WatchingType,
        events: EnumSet<IntDomainEvent>,
    ) {
        let watcher_cp = &mut self.watchers[watches];

        for event in events {
            let event_watcher = match event {
                IntDomainEvent::LowerBound => &mut watcher_cp.forward_watcher.lower_bound_watchers,
                IntDomainEvent::UpperBound => &mut watcher_cp.forward_watcher.upper_bound_watchers,
                IntDomainEvent::Assign => &mut watcher_cp.forward_watcher.assign_watchers,
                IntDomainEvent::Removal => &mut watcher_cp.forward_watcher.removal_watchers,
            };

            if !event_watcher.contains(&watcher) {
                event_watcher.push(watcher.clone());
            }
        }
    }

    fn watch_backtrack(
        &mut self,
        watcher: StoredType,
        watches: WatchingType,
        events: EnumSet<IntDomainEvent>,
    ) {
        let watcher_cp = &mut self.watchers[watches];

        for event in events {
            let backtrack_event_watchers = match event {
                IntDomainEvent::Assign => &mut watcher_cp.backtrack_watcher.assign_watchers,
                IntDomainEvent::LowerBound => {
                    &mut watcher_cp.backtrack_watcher.lower_bound_watchers
                }
                IntDomainEvent::UpperBound => {
                    &mut watcher_cp.backtrack_watcher.upper_bound_watchers
                }
                IntDomainEvent::Removal => &mut watcher_cp.backtrack_watcher.removal_watchers,
            };

            if !backtrack_event_watchers.contains(&watcher) {
                backtrack_event_watchers.push(watcher.clone())
            }
        }
    }
}

/// A watcher for backward and foward events
#[derive(Debug, Default)]
pub(super) struct ForwardBackwardWatcher<StoredType> {
    // FIXME measure performance of these vectors, they are treated as sets
    forward_watcher: DomainEventWatcher<StoredType>,
    backtrack_watcher: DomainEventWatcher<StoredType>,
}

/// Tracks watchers for the different kinds of [`IntDomainEvent`].
#[derive(Debug, Default)]
pub(super) struct DomainEventWatcher<StoredType> {
    lower_bound_watchers: StoredType,
    upper_bound_watchers: StoredType,
    assign_watchers: StoredType,
    removal_watchers: StoredType,
}
