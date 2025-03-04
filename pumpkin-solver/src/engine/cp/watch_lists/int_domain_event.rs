use std::fmt::Display;

use enumset::EnumSetType;

/// A description of the kinds of events that can happen on a domain variable.
#[derive(Debug, EnumSetType, Hash)]
pub enum IntDomainEvent {
    /// Event where an (integer) variable domain collapses to a single value.
    Assign,
    /// Event where an (integer) variable domain tightens the lower bound.
    LowerBound,
    /// Event where an (integer) variable domain tightens the upper bound.
    UpperBound,
    /// Event where an (integer) variable domain removes an inner value within the domain.
    /// N.B. this DomainEvent should not be subscribed to by itself!
    #[doc(hidden)]
    Removal,
}

impl Display for IntDomainEvent {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IntDomainEvent::Assign => write!(f, "[Event:Assign]"),
            IntDomainEvent::LowerBound => write!(f, "[Event:LB]"),
            IntDomainEvent::UpperBound => write!(f, "[Event:UB]"),
            IntDomainEvent::Removal => write!(f, "[Event:Remove]"),
        }
    }
}
