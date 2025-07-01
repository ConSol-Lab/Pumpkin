use enumset::enum_set;
use enumset::EnumSet;

use super::DomainEvent;

#[derive(Debug, Copy, Clone)]
pub(crate) struct DomainEvents {
    int_events: Option<EnumSet<DomainEvent>>,
}

impl DomainEvents {
    /// DomainEvents with both lower and upper bound tightening (but not other value removal).
    pub(crate) const BOUNDS: DomainEvents = DomainEvents::create_with_int_events(enum_set!(
        DomainEvent::LowerBound | DomainEvent::UpperBound
    ));
    // this is all options right now, but won't be once we add variables of other types
    /// DomainEvents with lower and upper bound tightening, assigning to a single value, and
    ///  single value removal.
    pub(crate) const ANY_INT: DomainEvents = DomainEvents::create_with_int_events(enum_set!(
        DomainEvent::Assign
            | DomainEvent::LowerBound
            | DomainEvent::UpperBound
            | DomainEvent::Removal
    ));
    pub(crate) const REMOVAL: DomainEvents =
        DomainEvents::create_with_int_events(enum_set!(DomainEvent::Removal));
    /// DomainEvents with only lower bound tightening.
    pub(crate) const LOWER_BOUND: DomainEvents =
        DomainEvents::create_with_int_events(enum_set!(DomainEvent::LowerBound));
    /// DomainEvents with only upper bound tightening.
    #[allow(unused, reason = "will be part of public API at some point")]
    pub(crate) const UPPER_BOUND: DomainEvents =
        DomainEvents::create_with_int_events(enum_set!(DomainEvent::UpperBound));
    /// DomainEvents with only assigning to a single value.
    pub(crate) const ASSIGN: DomainEvents =
        DomainEvents::create_with_int_events(enum_set!(DomainEvent::Assign));
    pub(crate) const ASSIGN_AND_REMOVAL: DomainEvents =
        DomainEvents::create_with_int_events(enum_set!(DomainEvent::Assign | DomainEvent::Removal));
}

impl DomainEvents {
    pub(crate) const fn create_with_int_events(int_events: EnumSet<DomainEvent>) -> DomainEvents {
        DomainEvents {
            int_events: Some(int_events),
        }
    }

    pub(crate) fn get_int_events(&self) -> EnumSet<DomainEvent> {
        self.int_events
            .expect("Tried to retrieve int_events when it was not initialized")
    }
}
