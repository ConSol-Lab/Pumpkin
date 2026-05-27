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
use crate::checkers::BoxedRetentionChecker;
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
use crate::propagation::EventRegistration;
use crate::propagators::reified_propagator::ReifiedChecker;
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
    fn create(
        self,
        context: PropagatorConstructorContext,
    ) -> (EventRegistration, Self::PropagatorImpl);
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

    /// Pending consistency checkers to be registered into [`State`] when this context is dropped.
    #[cfg(feature = "check-consistency")]
    pub(crate) pending_consistency_checkers: RefOrOwned<'a, Vec<(Scope, BoxedRetentionChecker)>>,

    /// Pending inference checkers to be registered into [`State`] when this context is dropped.
    #[cfg(feature = "check-propagations")]
    #[allow(
        clippy::type_complexity,
        reason = "it's not clear where the type alias would live"
    )]
    pub(crate) pending_inference_checkers:
        RefOrOwned<'a, Vec<(InferenceCode, Box<dyn InferenceChecker<Predicate>>)>>,
}

impl PropagatorConstructorContext<'_> {
    pub(crate) fn new<'a>(
        propagator_id: PropagatorId,
        state: &'a mut State,
    ) -> PropagatorConstructorContext<'a> {
        PropagatorConstructorContext {
            propagator_id,
            state,
            #[cfg(feature = "check-consistency")]
            pending_consistency_checkers: RefOrOwned::Owned(vec![]),
            #[cfg(feature = "check-propagations")]
            pending_inference_checkers: RefOrOwned::Owned(vec![]),
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
        checker: impl Into<BoxedRetentionChecker>,
    ) {
        #[cfg(feature = "check-consistency")]
        self.pending_consistency_checkers
            .push((scope.into(), checker.into()));

        // Avoid unused variable warning.
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

        // Avoid unused variable warning.
        #[cfg(not(feature = "check-propagations"))]
        {
            let _ = inference_code;
            let _ = checker;
        }
    }
}

impl Drop for PropagatorConstructorContext<'_> {
    fn drop(&mut self) {
        #[cfg(feature = "check-consistency")]
        for (scope, checker) in self.pending_consistency_checkers.drain(..) {
            self.state.add_consistency_checker(scope, checker);
        }

        #[cfg(feature = "check-propagations")]
        for (inference_code, checker) in self.pending_inference_checkers.drain(..) {
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
