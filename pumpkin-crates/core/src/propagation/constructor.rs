use super::Domains;
use super::LocalId;
use super::Propagator;
use super::PropagatorId;
use super::PropagatorVarId;
#[cfg(doc)]
use crate::Solver;
use crate::basic_types::PredicateId;
use crate::engine::Assignments;
use crate::engine::State;
use crate::engine::TrailedValues;
use crate::engine::notifications::Watchers;
#[cfg(doc)]
use crate::engine::variables::AffineView;
#[cfg(doc)]
use crate::engine::variables::DomainId;
use crate::predicates::Predicate;
#[cfg(doc)]
use crate::propagation::DomainEvent;
use crate::propagation::DomainEvents;
use crate::propagation::EventsToRegister;
use crate::propagation::RuntimeCheckers;
use crate::variables::IntegerVariable;

/// A propagator constructor creates a fully initialized instance of a [`Propagator`].
///
/// The constructor is responsible for:
/// 1) Indicating on which [`DomainEvent`]s the propagator should be enqueued (via the
///    [`PropagatorConstructorContext`]).
/// 2) Initialising the [`PropagatorConstructor::PropagatorImpl`] and its structures.
pub trait PropagatorConstructor {
    /// The propagator that is produced by this constructor.
    type PropagatorImpl: Propagator + Clone;

    /// Create the propagator instance from `Self`.
    fn create(
        self,
        context: PropagatorConstructorContext,
    ) -> ConstructedPropagator<Self::PropagatorImpl>;
}

/// The result of [`PropagatorConstructor::create`].
#[derive(Clone, Debug)]
pub struct ConstructedPropagator<P> {
    /// The domain events the propagator needs to be be registered for.
    pub registration: EventsToRegister,
    /// Any runtime checkers that verify the propagator's implementation.
    pub checkers: RuntimeCheckers,
    /// The propagator
    pub propagator: P,
}

/// [`PropagatorConstructorContext`] is used when [`Propagator`]s are initialised after creation.
///
/// It represents a communication point between the [`Solver`] and the [`Propagator`].
/// Propagators use the [`PropagatorConstructorContext`] to register to domain changes
/// of variables and to retrieve the current bounds of variables.
#[derive(Debug)]
pub struct PropagatorConstructorContext<'a> {
    state: &'a mut State,
    pub(crate) propagator_id: PropagatorId,
}

impl PropagatorConstructorContext<'_> {
    pub(crate) fn new<'a>(
        propagator_id: PropagatorId,
        state: &'a mut State,
    ) -> PropagatorConstructorContext<'a> {
        PropagatorConstructorContext {
            propagator_id,
            state,
        }
    }

    /// Get domain information.
    pub fn domains(&mut self) -> Domains<'_> {
        Domains::new(&self.state.assignments, &mut self.state.trailed_values)
    }

    /// Register the propagator to be enqueued when the given [`Predicate`] becomes true.
    /// Returns the [`PredicateId`] used by the solver to track the predicate.
    pub fn register_predicate(&mut self, predicate: Predicate) -> PredicateId {
        self.state.notification_engine.watch_predicate(
            predicate,
            self.propagator_id,
            &mut self.state.trailed_values,
            &self.state.assignments,
        )
    }

    /// Subscribes the propagator to the given [`DomainEvents`] when they are undone during
    /// backtracking. This method is complementary to [`PropagatorConstructorContext::register`],
    /// the [`LocalId`]s provided to both of these method should be the same for the same variable.
    ///
    /// The domain events determine when [`Propagator::notify_backtrack()`] will be called on the
    /// propagator. The [`LocalId`] is internal information related to the propagator,
    /// which is used when calling [`Propagator::notify_backtrack()`] to identify the variable.
    ///
    /// Each variable *must* have a unique [`LocalId`]. Most often this would be its index of the
    /// variable in the internal array of variables.
    ///
    /// Note that the [`LocalId`] is used to differentiate between [`DomainId`]s and
    /// [`AffineView`]s.
    pub fn register_backtrack<Var: IntegerVariable>(
        &mut self,
        var: Var,
        domain_events: DomainEvents,
        local_id: LocalId,
    ) {
        let propagator_var = PropagatorVarId {
            propagator: self.propagator_id,
            variable: local_id,
        };

        let mut watchers = Watchers::new(propagator_var, &mut self.state.notification_engine);
        var.watch_all_backtrack(&mut watchers, domain_events.events());
    }

    /// Reborrow the current context to a new value with a shorter lifetime. Should be used when
    /// passing `Self` to another function that takes ownership, but the value is still needed
    /// afterwards.
    pub fn reborrow(&mut self) -> PropagatorConstructorContext<'_> {
        PropagatorConstructorContext {
            propagator_id: self.propagator_id,
            state: self.state,
        }
    }
}

mod private {
    use super::*;
    use crate::propagation::HasAssignments;

    impl HasAssignments for PropagatorConstructorContext<'_> {
        fn assignments(&self) -> &Assignments {
            &self.state.assignments
        }

        fn trailed_values(&self) -> &TrailedValues {
            &self.state.trailed_values
        }

        fn trailed_values_mut(&mut self) -> &mut TrailedValues {
            &mut self.state.trailed_values
        }
    }
}
