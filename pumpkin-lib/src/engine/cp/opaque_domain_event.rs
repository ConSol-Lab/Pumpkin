use super::BacktrackEvent;
use crate::engine::IntDomainEvent;

/// A wrapper for a domain event, which forces the propagator implementation to map the event
/// through the variable view.
#[derive(Clone, Debug, Copy)]
pub struct OpaqueDomainEvent(IntDomainEvent);

impl From<IntDomainEvent> for OpaqueDomainEvent {
    fn from(event: IntDomainEvent) -> Self {
        OpaqueDomainEvent(event)
    }
}

impl OpaqueDomainEvent {
    pub(crate) fn unwrap(self) -> IntDomainEvent {
        self.0
    }
}

/// A wrapper for a domain event while backtrcking, which forces the propagator implementation to
/// map the event through the variable view.
#[derive(Clone, Debug, Copy)]
pub struct OpaqueBacktrackDomainEvent(BacktrackEvent);

impl From<BacktrackEvent> for OpaqueBacktrackDomainEvent {
    fn from(event: BacktrackEvent) -> Self {
        OpaqueBacktrackDomainEvent(event)
    }
}

impl OpaqueBacktrackDomainEvent {
    pub(crate) fn unwrap(self) -> BacktrackEvent {
        self.0
    }
}
