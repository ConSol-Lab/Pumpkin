use enumset::enum_set;
use enumset::EnumSet;

use crate::engine::BooleanDomainEvent;
use crate::engine::IntDomainEvent;

impl DomainEvents {
    /// DomainEvents for assigning true to literal
    pub const ASSIGNED_TRUE: DomainEvents =
        DomainEvents::create_with_bool_events(enum_set!(BooleanDomainEvent::AssignedTrue));
    /// DomainEvents for assigning false to literal
    pub const ASSIGNED_FALSE: DomainEvents =
        DomainEvents::create_with_bool_events(enum_set!(BooleanDomainEvent::AssignedFalse));
    /// DomainEvents for assigning true and false to literal
    pub const ANY_BOOL: DomainEvents = DomainEvents::create_with_bool_events(enum_set!(
        BooleanDomainEvent::AssignedTrue | BooleanDomainEvent::AssignedFalse
    ));
    /// DomainEvents with both lower and upper bound tightening (but not other value removal).
    pub const BOUNDS: DomainEvents = DomainEvents::create_with_int_events(enum_set!(
        IntDomainEvent::LowerBound | IntDomainEvent::UpperBound
    ));
    // this is all options right now, but won't be once we add variables of other types
    /// DomainEvents with lower and upper bound tightening, assigning to a single value, and
    ///  single value removal.
    pub const ANY_INT: DomainEvents = DomainEvents::create_with_int_events(enum_set!(
        IntDomainEvent::Assign
            | IntDomainEvent::LowerBound
            | IntDomainEvent::UpperBound
            | IntDomainEvent::Removal
    ));
    /// DomainEvents with only lower bound tightening.
    pub const LOWER_BOUND: DomainEvents =
        DomainEvents::create_with_int_events(enum_set!(IntDomainEvent::LowerBound));
    /// DomainEvents with only upper bound tightening.
    pub const UPPER_BOUND: DomainEvents =
        DomainEvents::create_with_int_events(enum_set!(IntDomainEvent::UpperBound));
    /// DomainEvents with only assigning to a single value.
    pub const ASSIGN: DomainEvents =
        DomainEvents::create_with_int_events(enum_set!(IntDomainEvent::Assign));
}

#[derive(Debug, Copy, Clone)]
pub struct DomainEvents {
    int_events: Option<EnumSet<IntDomainEvent>>,
    boolean_events: Option<EnumSet<BooleanDomainEvent>>,
}

impl DomainEvents {
    pub(crate) const fn create_with_int_events(
        int_events: EnumSet<IntDomainEvent>,
    ) -> DomainEvents {
        DomainEvents {
            int_events: Some(int_events),
            boolean_events: None,
        }
    }

    pub(crate) const fn create_with_bool_events(
        boolean_events: EnumSet<BooleanDomainEvent>,
    ) -> DomainEvents {
        DomainEvents {
            int_events: None,
            boolean_events: Some(boolean_events),
        }
    }

    pub(crate) fn get_int_events(&self) -> EnumSet<IntDomainEvent> {
        self.int_events
            .expect("Tried to retrieve int_events when it was not initialized")
    }

    pub(crate) fn get_bool_events(&self) -> EnumSet<BooleanDomainEvent> {
        self.boolean_events
            .expect("Tried to retrieve boolean_events when it was not initialized")
    }
}
