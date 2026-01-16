use super::DomainEvent;
#[cfg(doc)]
use crate::engine::variables::IntegerVariable;

/// A [`DomainEvent`] that happened in the solver.
///
/// Obtain the event from the perspective of a variable through [`IntegerVariable::unpack_event`].
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
