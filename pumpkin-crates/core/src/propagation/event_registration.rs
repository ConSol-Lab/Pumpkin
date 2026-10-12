use enumset::EnumSet;

use crate::predicates::Predicate;
use crate::propagation::DomainEvent;
use crate::propagation::DomainEvents;
use crate::propagation::LocalId;
use crate::variables::DomainId;
#[cfg(doc)]
use crate::variables::Literal;

/// Anything that can subscribe to domain events.
///
/// Typically these are variables.
pub trait EventTarget {
    /// Indicate that `self` should be registered for the given domain events as the given local ID.
    fn register(
        &self,
        registration: &mut impl EventDispatcher,
        events: EnumSet<DomainEvent>,
        local_id: LocalId,
    );
}

/// The interface to a component that needs to know which variables care about which events.
pub trait EventDispatcher {
    // This is a separate trait to isolate the event registration from the rest of the solver.
    // That isolation is beneficial when writing tests, as individual components can easily be
    // mocked without needing to set up an entire state/solver.

    /// Register the [`DomainId`] with the given [`LocalId`] on the given [`DomainEvents`].
    ///
    /// It is possible to register the same [`DomainId`] with different [`LocalId`]s. This may
    /// happen when the propagator uses multiple local IDs for different events for the same
    /// variable. Or when different views over the same domain are used in a propagator.
    fn register(&mut self, domain_id: DomainId, events: EnumSet<DomainEvent>, local_id: LocalId);

    /// Register a [`Literal`] over the given [`Predicate`] with the given [`LocalId`] on the given
    /// [`DomainEvents`].
    ///
    /// The literal is treated as a 0-1 variable, which is 1 when the predicate is true and 0 when
    /// the predicate is false.
    fn register_literal(
        &mut self,
        predicate: Predicate,
        events: EnumSet<DomainEvent>,
        local_id: LocalId,
    );
}

/// Contains all the events and domains that a propagator needs to be enqueued for.
#[derive(Clone, Debug, Default)]
pub struct EventsToRegister {
    domains: Vec<(DomainId, EnumSet<DomainEvent>, LocalId)>,
    /// The literals, identified by their predicate.
    literals: Vec<(Predicate, EnumSet<DomainEvent>, LocalId)>,
}

impl EventsToRegister {
    /// Create an [`EventsToRegister`] without any variables.
    ///
    /// This is the uncommon case. Without registering for variable events, a propagator will never
    /// be enqueued automatically. However, certain propagators like, e.g., compound propagators,
    /// may not be able to register during construction in which case they will be enqueued
    /// explicitly when a constraint is added to them. The nogood propagator is an example of such a
    /// propagator.
    pub fn empty() -> EventsToRegister {
        EventsToRegister::default()
    }

    /// Create a new [`EventsToRegisterBuilder`].
    ///
    /// If no event registrations will be made, use [`EventsToRegister::empty`] instead.
    /// Calling [`EventsToRegisterBuilder::build`] without any registrations will cause a panic.
    pub fn builder() -> EventsToRegisterBuilder {
        EventsToRegisterBuilder {
            registrations: EventsToRegister::default(),
        }
    }

    /// Add a new event registration to an existing instance of self.
    ///
    /// # Example
    ///
    /// ```
    /// use pumpkin_core::propagation::DomainEvents;
    /// use pumpkin_core::propagation::EventsToRegister;
    /// use pumpkin_core::propagation::LocalId;
    /// use pumpkin_core::variables::DomainId;
    ///
    /// let v1 = DomainId::new(0);
    /// let v2 = DomainId::new(0);
    /// let mut registration = EventsToRegister::builder()
    ///     .add(&v1, DomainEvents::ANY_INT, LocalId::from(0))
    ///     .build();
    ///
    /// // Extend the events to register with another variable.
    /// registration.add(&v2, DomainEvents::ANY_INT, LocalId::from(1));
    /// ```
    pub fn add(
        &mut self,
        target: &impl EventTarget,
        domain_events: DomainEvents,
        local_id: LocalId,
    ) {
        target.register(self, domain_events.events(), local_id);
    }

    /// Iterate the registrations of domains already made.
    pub fn iter(&self) -> impl ExactSizeIterator<Item = (DomainId, EnumSet<DomainEvent>, LocalId)> {
        self.domains.iter().copied()
    }

    /// Iterate the registrations of [`Literal`]s (identified by their predicate) already made.
    pub fn iter_literals(
        &self,
    ) -> impl ExactSizeIterator<Item = (Predicate, EnumSet<DomainEvent>, LocalId)> {
        self.literals.iter().copied()
    }
}

impl EventDispatcher for EventsToRegister {
    fn register(&mut self, domain_id: DomainId, events: EnumSet<DomainEvent>, local_id: LocalId) {
        self.domains.push((domain_id, events, local_id));
    }

    fn register_literal(
        &mut self,
        predicate: Predicate,
        events: EnumSet<DomainEvent>,
        local_id: LocalId,
    ) {
        self.literals.push((predicate, events, local_id));
    }
}

/// Used to construct an [`EventsToRegister`] for heterogeneous [`EventTarget`] implementations.
///
/// See [`EventsToRegister::builder`] for a usage example.
#[derive(Clone, Debug)]
pub struct EventsToRegisterBuilder {
    registrations: EventsToRegister,
}

impl EventsToRegisterBuilder {
    /// Add a new event registration.
    ///
    /// # Example
    ///
    /// ```
    /// use pumpkin_core::propagation::DomainEvents;
    /// use pumpkin_core::propagation::EventsToRegister;
    /// use pumpkin_core::propagation::LocalId;
    /// use pumpkin_core::variables::DomainId;
    ///
    /// let v1 = DomainId::new(0);
    /// let v2 = DomainId::new(0);
    /// let registration = EventsToRegister::builder()
    ///     .add(&v1, DomainEvents::ANY_INT, LocalId::from(0))
    ///     .add(&v2, DomainEvents::ANY_INT, LocalId::from(1))
    ///     .build();
    /// ```
    pub fn add(
        mut self,
        target: &impl EventTarget,
        domain_events: DomainEvents,
        local_id: LocalId,
    ) -> Self {
        self.registrations.add(target, domain_events, local_id);
        self
    }

    /// Finish constructing the [`EventsToRegister`].
    ///
    /// If no variables are registered, then this panics. If no variables can be registered during
    /// construction, use [`EventsToRegister::empty`].
    pub fn build(self) -> EventsToRegister {
        assert!(
            !self.registrations.domains.is_empty() || !self.registrations.literals.is_empty(),
            "did not register for any events"
        );

        self.registrations
    }
}
