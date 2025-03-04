use std::fmt::Display;

use enumset::EnumSetType;

/// A description of the kinds of events that can happen on a domain variable.
#[derive(Debug, EnumSetType, Hash)]
pub(crate) enum PredicateDomainEvent {
    /// Event where a predicate is assigned to true
    AssignTrue,
    /// Event where a predicate is assigned to false
    AssignFalse,
}

impl Display for PredicateDomainEvent {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PredicateDomainEvent::AssignTrue => write!(f, "[Event::True]"),
            PredicateDomainEvent::AssignFalse => write!(f, "[Event::False]"),
        }
    }
}
