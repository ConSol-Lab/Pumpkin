use enumset::EnumSet;

use crate::propagation::DomainEvent;
use crate::propagation::DomainEvents;
use crate::propagation::LocalId;
use crate::variables::DomainId;

/// Anything that can subscribe to domain events.
pub trait EventTarget {
    /// Add a registration of self for the given domain events with a local id.
    fn register(
        &self,
        registration: &mut impl EventDispatcher,
        events: EnumSet<DomainEvent>,
        local_id: LocalId,
    );
}

pub trait EventDispatcher {
    /// Register the [`DomainId`] with the given [`LocalId`] on the given [`DomainEvents`].
    fn register(&mut self, domain_id: DomainId, events: EnumSet<DomainEvent>, local_id: LocalId);
}

/// Contains all the events and domains that a propagator needs to be enqueued for.
#[derive(Clone, Debug)]
pub struct EventsToRegister(Vec<(DomainId, EnumSet<DomainEvent>, LocalId)>);

impl EventsToRegister {
    /// Create an [`EventRegistration`] without any variables.
    ///
    /// This is the uncommon case. Without registering for variable events, a propagator will never
    /// be enqueued.
    pub fn empty() -> EventsToRegister {
        EventsToRegister(vec![])
    }

    /// Create a new [`EventRegistrationBuilder`].
    ///
    /// If no event registrations will be made, use [`EventRegistration::empty`] instead.
    /// Calling [`EventRegistrationBuilder::build`] without any registrations will cause a panic.
    ///
    /// # Example
    ///
    /// ```
    /// use pumpkin_core::propagation::DomainEvents;
    /// use pumpkin_core::propagation::EventRegistration;
    /// use pumpkin_core::propagation::LocalId;
    /// use pumpkin_core::variables::DomainId;
    ///
    /// let v1 = DomainId::new(0);
    /// let v2 = DomainId::new(0);
    /// let registration = EventRegistration::builder()
    ///     .add(&v1, DomainEvents::ANY_INT, LocalId::from(0))
    ///     .add(&v2, DomainEvents::ANY_INT, LocalId::from(1))
    ///     .build();
    /// ```
    pub fn builder() -> EventRegistrationBuilder {
        EventRegistrationBuilder {
            registrations: EventsToRegister(vec![]),
        }
    }

    /// Add a new event registration.
    pub fn add(
        &mut self,
        target: &impl EventTarget,
        domain_events: DomainEvents,
        local_id: LocalId,
    ) {
        target.register(self, domain_events.events(), local_id);
    }

    /// Iterate the registrations already made.
    pub fn iter(&self) -> impl ExactSizeIterator<Item = (DomainId, EnumSet<DomainEvent>, LocalId)> {
        self.0.iter().copied()
    }
}

impl EventDispatcher for EventsToRegister {
    fn register(&mut self, domain_id: DomainId, events: EnumSet<DomainEvent>, local_id: LocalId) {
        self.0.push((domain_id, events, local_id));
    }
}

/// Used to construct an [`EventRegistration`] for heterogeneous [`EventTarget`] implementations.
///
/// See [`EventRegistration::builder`] for a usage example.
#[derive(Clone, Debug)]
pub struct EventRegistrationBuilder {
    registrations: EventsToRegister,
}

impl EventRegistrationBuilder {
    /// Add a new event registration.
    pub fn add(
        mut self,
        target: &impl EventTarget,
        domain_events: DomainEvents,
        local_id: LocalId,
    ) -> Self {
        self.registrations.add(target, domain_events, local_id);
        self
    }

    /// Finish constructing the [`EventRegistration`].
    ///
    /// If no variables are registered, then this panics. If no variables can be registered during
    /// construction, use [`EventRegistration::empty`].
    pub fn build(self) -> EventsToRegister {
        assert!(
            !self.registrations.0.is_empty(),
            "did not register for any events"
        );

        self.registrations
    }
}
