use std::ops::Deref;
use std::ops::DerefMut;

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
use crate::proof::ConstraintTag;
use crate::proof::InferenceCode;
use crate::proof::InferenceLabel;
use crate::propagation::DomainEvents;
use crate::variables::IntegerVariable;

/// A propagator constructor creates a fully initialized instance of a [`Propagator`].
///
/// The constructor is responsible for indicating on which events the propagator should be
/// enqueued. Additionally, the propagator can be initialized with values that come from the state
/// of the solver.
pub trait PropagatorConstructor {
    /// The propagator that is produced by this constructor.
    type PropagatorImpl: Propagator + Clone;

    /// Create the propagator instance from `Self`.
    fn create(self, context: PropagatorConstructorContext) -> Self::PropagatorImpl;
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

    /// A [`LocalId`] that is guaranteed not to be used to register any variables yet. This is
    /// either a reference or an owned value, to support
    /// [`PropagatorConstructorContext::reborrow`].
    next_local_id: RefOrOwned<'a, LocalId>,
}

impl PropagatorConstructorContext<'_> {
    pub(crate) fn new<'a>(
        propagator_id: PropagatorId,
        state: &'a mut State,
    ) -> PropagatorConstructorContext<'a> {
        PropagatorConstructorContext {
            next_local_id: RefOrOwned::Owned(LocalId::from(0)),
            propagator_id,
            state,
        }
    }

    /// Get domain information.
    pub fn domains(&self) -> Domains<'_> {
        Domains::new(&self.state.assignments, &self.state.trailed_values)
    }

    /// Subscribes the propagator to the given [`DomainEvents`].
    ///
    /// The domain events determine when [`Propagator::notify()`] will be called on the propagator.
    /// The [`LocalId`] is internal information related to the propagator,
    /// which is used when calling [`Propagator::notify()`] to identify the variable.
    ///
    /// Each variable *must* have a unique [`LocalId`]. Most often this would be its index of the
    /// variable in the internal array of variables.
    pub fn register(
        &mut self,
        var: impl IntegerVariable,
        domain_events: DomainEvents,
        local_id: LocalId,
    ) {
        let propagator_var = PropagatorVarId {
            propagator: self.propagator_id,
            variable: local_id,
        };

        self.update_next_local_id(local_id);

        let mut watchers = Watchers::new(propagator_var, &mut self.state.notification_engine);
        var.watch_all(&mut watchers, domain_events.events());
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

        self.update_next_local_id(local_id);

        let mut watchers = Watchers::new(propagator_var, &mut self.state.notification_engine);
        var.watch_all_backtrack(&mut watchers, domain_events.events());
    }

    /// Create a new [`InferenceCode`]. These codes are required to identify specific propagations
    /// in the solver and the proof.
    pub(crate) fn create_inference_code(
        &mut self,
        constraint_tag: ConstraintTag,
        inference_label: impl InferenceLabel,
    ) -> InferenceCode {
        self.state
            .create_inference_code(constraint_tag, inference_label)
    }

    /// Get a new [`LocalId`] which is guaranteed to be unused.
    pub(crate) fn get_next_local_id(&self) -> LocalId {
        *self.next_local_id.deref()
    }

    /// Reborrow the current context to a new value with a shorter lifetime. Should be used when
    /// passing `Self` to another function that takes ownership, but the value is still needed
    /// afterwards.
    pub(crate) fn reborrow(&mut self) -> PropagatorConstructorContext<'_> {
        PropagatorConstructorContext {
            propagator_id: self.propagator_id,
            next_local_id: match &mut self.next_local_id {
                RefOrOwned::Ref(next_local_id) => RefOrOwned::Ref(next_local_id),
                RefOrOwned::Owned(next_local_id) => RefOrOwned::Ref(next_local_id),
            },
            state: self.state,
        }
    }

    /// Set the next local id to be at least one more than the largest encountered local id.
    fn update_next_local_id(&mut self, local_id: LocalId) {
        let next_local_id = (*self.next_local_id.deref()).max(LocalId::from(local_id.unpack() + 1));

        *self.next_local_id.deref_mut() = next_local_id;
    }
}

/// Either owns a value or has a mutable reference to a value.
///
/// Used to store data in a reborrowed context that needs to be 'shared' with the original context
/// that was reborrowed from. For example, when dropping a reborrowed context, we want
/// [`PropagatorConstructorContext::get_next_local_id`] in the original context to 'know' about the
/// registered local ids in the reborrowed context.
#[derive(Debug)]
enum RefOrOwned<'a, T> {
    Ref(&'a mut T),
    Owned(T),
}

impl<T> Deref for RefOrOwned<'_, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        match self {
            RefOrOwned::Ref(reference) => reference,
            RefOrOwned::Owned(value) => value,
        }
    }
}

impl<T> DerefMut for RefOrOwned<'_, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        match self {
            RefOrOwned::Ref(reference) => reference,
            RefOrOwned::Owned(value) => value,
        }
    }
}

mod private {
    use super::*;
    use crate::propagation::HasAssignments;
    use crate::propagation::HasTrailedValues;

    impl HasAssignments for PropagatorConstructorContext<'_> {
        fn assignments(&self) -> &Assignments {
            &self.state.assignments
        }
    }

    impl HasTrailedValues for PropagatorConstructorContext<'_> {
        fn trailed_values(&self) -> &TrailedValues {
            &self.state.trailed_values
        }

        fn trailed_values_mut(&mut self) -> &mut TrailedValues {
            &mut self.state.trailed_values
        }
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::variables::DomainId;

    #[test]
    fn reborrowing_remembers_next_local_id() {
        let mut state = State::default();
        state.notification_engine.grow();

        let mut c1 = PropagatorConstructorContext::new(PropagatorId(0), &mut state);

        let mut c2 = c1.reborrow();
        c2.register(DomainId::new(0), DomainEvents::ANY_INT, LocalId::from(1));

        assert_eq!(LocalId::from(2), c1.get_next_local_id());
    }
}
