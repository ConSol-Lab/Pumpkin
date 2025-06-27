use super::DomainEvent;

/// A wrapper for a domain event, which forces the propagator implementation to map the event
/// through the variable view.
#[derive(Clone, Debug, Copy)]
pub struct OpaqueDomainEvent(DomainEvent);

impl From<DomainEvent> for OpaqueDomainEvent {
    fn from(event: DomainEvent) -> Self {
        OpaqueDomainEvent(event)
    }
}

impl OpaqueDomainEvent {
    pub(crate) fn unwrap(self) -> DomainEvent {
        self.0
    }
}
