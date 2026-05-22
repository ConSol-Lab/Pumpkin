use std::ops::Deref;
use std::ops::DerefMut;

use pumpkin_checking::InferenceChecker;

use super::Domains;
use super::LocalId;
use super::Propagator;
use super::PropagatorId;
use super::PropagatorVarId;
#[cfg(doc)]
use crate::Solver;
use crate::basic_types::PredicateId;
use crate::basic_types::RefOrOwned;
use crate::checkers::BoxedConsistencyChecker;
use crate::checkers::Scope;
use crate::engine::Assignments;
use crate::engine::State;
use crate::engine::TrailedValues;
use crate::engine::notifications::Watchers;
#[cfg(doc)]
use crate::engine::variables::AffineView;
#[cfg(doc)]
use crate::engine::variables::DomainId;
use crate::predicates::Predicate;
use crate::proof::InferenceCode;
#[cfg(doc)]
use crate::propagation::DomainEvent;
use crate::propagation::DomainEvents;
use crate::variables::IntegerVariable;

/// A propagator constructor creates a fully initialized instance of a [`Propagator`].
///
/// The constructor is responsible for:
/// 1) Indicating on which [`DomainEvent`]s the propagator should be enqueued (via the
///    [`PropagatorConstructorContext`]).
/// 2) Initialising the [`PropagatorConstructor::PropagatorImpl`] and its structures.
///
/// Inference checkers and consistency checkers should be registered inside [`Self::create`] via
/// [`PropagatorConstructorContext::add_inference_checker`] and
/// [`PropagatorConstructorContext::add_consistency_checker`].
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
    pub(crate) state: &'a mut State,
    pub(crate) propagator_id: PropagatorId,

    /// A [`LocalId`] that is guaranteed not to be used to register any variables yet. This is
    /// either a reference or an owned value, to support
    /// [`PropagatorConstructorContext::reborrow`].
    pub(crate) next_local_id: RefOrOwned<'a, LocalId>,

    /// Marker to indicate whether the constructor registered for at least one domain event or
    /// predicate becoming assigned. If not, the [`Drop`] implementation will cause a panic.
    pub(crate) did_register: RefOrOwned<'a, bool>,

    /// Pending consistency checkers to be registered into [`State`] when this context is dropped.
    #[cfg(feature = "check-consistency")]
    pub(crate) pending_consistency_checkers: RefOrOwned<'a, Vec<(Scope, BoxedConsistencyChecker)>>,

    /// Pending inference checkers to be registered into [`State`] when this context is dropped.
    #[cfg(feature = "check-propagations")]
    pub(crate) pending_inference_checkers:
        RefOrOwned<'a, Vec<(InferenceCode, Box<dyn InferenceChecker<Predicate>>)>>,
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
            did_register: RefOrOwned::Owned(false),
            #[cfg(feature = "check-consistency")]
            pending_consistency_checkers: RefOrOwned::Owned(vec![]),
            #[cfg(feature = "check-propagations")]
            pending_inference_checkers: RefOrOwned::Owned(vec![]),
        }
    }

    /// Indicate that the constructor is deliberately not registering the propagator to be enqueued
    /// at any time.
    ///
    /// If this is called and later a registration happens, then the registration will still go
    /// through. Calling this function only prevents the crash if no registration happens.
    pub fn will_not_register_any_events(&mut self) {
        *self.did_register = true;
    }

    /// Get domain information.
    pub fn domains(&mut self) -> Domains<'_> {
        Domains::new(&self.state.assignments, &mut self.state.trailed_values)
    }

    /// Subscribes the propagator to the given [`DomainEvents`].
    ///
    /// The domain events determine when [`Propagator::notify()`] will be called on the propagator.
    /// The [`LocalId`] is internal information related to the propagator,
    /// which is used when calling [`Propagator::notify()`] to identify the variable.
    ///
    /// Each variable *must* have a unique [`LocalId`]. Most often this would be its index of the
    /// variable in the internal array of variables.
    ///
    /// Duplicate registrations are ignored.
    pub fn register(
        &mut self,
        var: impl IntegerVariable,
        domain_events: DomainEvents,
        local_id: LocalId,
    ) {
        self.will_not_register_any_events();

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
        self.will_not_register_any_events();

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

    /// Get a new [`LocalId`] which is guaranteed to be unused.
    pub(crate) fn get_next_local_id(&self) -> LocalId {
        *self.next_local_id.deref()
    }

    /// Reborrow the current context to a new value with a shorter lifetime. Should be used when
    /// passing `Self` to another function that takes ownership, but the value is still needed
    /// afterwards.
    pub fn reborrow(&mut self) -> PropagatorConstructorContext<'_> {
        PropagatorConstructorContext {
            propagator_id: self.propagator_id,
            next_local_id: self.next_local_id.reborrow(),
            did_register: self.did_register.reborrow(),
            state: self.state,
            #[cfg(feature = "check-consistency")]
            pending_consistency_checkers: self.pending_consistency_checkers.reborrow(),
            #[cfg(feature = "check-propagations")]
            pending_inference_checkers: self.pending_inference_checkers.reborrow(),
        }
    }

    /// Add a consistency checker for the given constraint and scope.
    ///
    /// If the `check-consistency` feature is not enabled, this is a no-op.
    pub fn add_consistency_checker(
        &mut self,
        scope: impl Into<Scope>,
        checker: impl Into<BoxedConsistencyChecker>,
    ) {
        #[cfg(feature = "check-consistency")]
        self.pending_consistency_checkers
            .push((scope.into(), checker.into()));

        #[cfg(not(feature = "check-consistency"))]
        {
            let _ = scope;
            let _ = checker;
        }
    }

    /// Add an inference checker for inferences produced by the propagator.
    ///
    /// If the `check-propagations` feature is not enabled, this is a no-op.
    pub fn add_inference_checker(
        &mut self,
        inference_code: InferenceCode,
        checker: Box<dyn InferenceChecker<Predicate>>,
    ) {
        #[cfg(feature = "check-propagations")]
        self.pending_inference_checkers
            .push((inference_code, checker));

        #[cfg(not(feature = "check-propagations"))]
        {
            let _ = inference_code;
            let _ = checker;
        }
    }

    /// Set the next local id to be at least one more than the largest encountered local id.
    fn update_next_local_id(&mut self, local_id: LocalId) {
        let next_local_id = (*self.next_local_id.deref()).max(LocalId::from(local_id.unpack() + 1));

        *self.next_local_id.deref_mut() = next_local_id;
    }
}

impl Drop for PropagatorConstructorContext<'_> {
    fn drop(&mut self) {
        if std::thread::panicking() {
            // If we are already unwinding due to a panic, we do not want to trigger another one.
            return;
        }

        let did_register = match self.did_register {
            // If we are in a reborrowed context, we do not want to enforce registration or drain
            // pending checkers (the root context handles this).
            RefOrOwned::Ref(_) => return,

            RefOrOwned::Owned(did_register) => did_register,
        };

        if !did_register {
            panic!(
                "Propagator did not register to be enqueued. If this is intentional, call PropagatorConstructorContext::will_not_register_any_events()."
            );
        }

        #[cfg(feature = "check-consistency")]
        for (scope, checker) in std::mem::take(&mut *self.pending_consistency_checkers) {
            self.state.add_consistency_checker(scope, checker);
        }

        #[cfg(feature = "check-propagations")]
        for (inference_code, checker) in std::mem::take(&mut *self.pending_inference_checkers) {
            self.state.add_inference_checker(inference_code, checker);
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

#[cfg(test)]
mod tests {

    use super::*;
    use crate::variables::DomainId;

    #[test]
    #[should_panic]
    fn panic_when_no_registration_happened() {
        let mut state = State::default();
        state.notification_engine.grow();

        let _c1 = PropagatorConstructorContext::new(PropagatorId(0), &mut state);
    }

    #[test]
    fn do_not_panic_if_told_no_registration_will_happen() {
        let mut state = State::default();
        state.notification_engine.grow();

        let mut ctx = PropagatorConstructorContext::new(PropagatorId(0), &mut state);
        ctx.will_not_register_any_events();
    }

    #[test]
    fn do_not_panic_if_no_registration_happens_in_reborrowed() {
        let mut state = State::default();
        state.notification_engine.grow();

        let mut ctx = PropagatorConstructorContext::new(PropagatorId(0), &mut state);
        let ctx2 = ctx.reborrow();
        drop(ctx2);

        ctx.will_not_register_any_events();
    }

    #[test]
    fn reborrowing_remembers_next_local_id() {
        let mut state = State::default();
        state.notification_engine.grow();

        let mut c1 = PropagatorConstructorContext::new(PropagatorId(0), &mut state);
        c1.will_not_register_any_events();

        let mut c2 = c1.reborrow();
        c2.register(DomainId::new(0), DomainEvents::ANY_INT, LocalId::from(1));
        drop(c2);

        assert_eq!(LocalId::from(2), c1.get_next_local_id());
    }
}
