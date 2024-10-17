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
