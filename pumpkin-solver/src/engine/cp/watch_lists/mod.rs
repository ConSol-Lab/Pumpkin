mod domain_event_watch_list;
mod int_domain_event;
mod predicate_domain_event;
mod predicate_event_watch_list;
mod watch_list_cp;
mod watch_list_manager;

pub(crate) use domain_event_watch_list::*;
use enumset::EnumSet;
use enumset::EnumSetType;
pub(crate) use int_domain_event::*;
pub(crate) use predicate_domain_event::*;
pub(crate) use predicate_event_watch_list::*;
pub(crate) use watch_list_cp::*;
pub(crate) use watch_list_manager::*;

/// A trait specifying the behaviour of watch lists.
///
/// Allows the following
/// - Determining whether the watch list is active
/// - Determining which elements were affected by an event
/// - Adding watcher for events on elements
pub(crate) trait WatchList<WatchingType, StoredType, Event: EnumSetType> {
    /// Grows the watch list by a single element
    fn grow(&mut self);

    /// Returns whether the watcher is watching any forward events
    ///
    /// By default this method will return true
    #[allow(dead_code, reason = "Will be part of the public API")]
    fn is_watching_anything(&self) -> bool {
        true
    }

    /// Returns whether the watcher is watching any backward events
    fn is_watching_anything_backtrack(&self) -> bool {
        true
    }

    /// Indicates that the watch list is watching forward events
    fn watches(&mut self) {}

    /// Indicates that the wathc list is watching backward events
    fn watches_backtrack(&mut self) {}

    /// Returns the affected stored elements if a forward event took place for domain
    fn get_affected(&self, event: Event, domain: WatchingType) -> &[StoredType];

    /// Returns the affected stored elements if a backward event took place for domain

    fn get_affected_backtrack(&self, _event: Event, _domain: WatchingType) -> &[StoredType] {
        unimplemented!()
    }

    /// Stores the `watcher` which is watching `watches` when the forward `events` occur
    fn watch(&mut self, watcher: StoredType, watches: WatchingType, events: EnumSet<Event>);

    /// Stores the `watcher` which is watching `watches` when the backward `events` occur
    fn watch_backtrack(
        &mut self,
        _watcher: StoredType,
        _watches: WatchingType,
        _events: EnumSet<IntDomainEvent>,
    ) {
        unimplemented!()
    }
}
