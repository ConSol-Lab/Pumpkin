use pumpkin_checking::InferenceChecker;

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
use crate::proof::InferenceCode;
#[cfg(doc)]
use crate::propagation::DomainEvent;
use crate::propagation::DomainEvents;
use crate::propagation::EventsToRegister;
use crate::propagators::reified_propagator::ReifiedChecker;
use crate::variables::IntegerVariable;
use crate::variables::Literal;

/// A propagator constructor creates a fully initialized instance of a [`Propagator`].
///
/// The constructor is responsible for:
/// 1) Indicating on which [`DomainEvent`]s the propagator should be enqueued (via the
///    [`PropagatorConstructorContext`]).
/// 2) Initialising the [`PropagatorConstructor::PropagatorImpl`] and its structures.
pub trait PropagatorConstructor {
    /// The propagator that is produced by this constructor.
    type PropagatorImpl: Propagator + Clone;

    /// Add inference checkers to the solver if applicable.
    ///
    /// If the `check-propagations` feature is turned on, then the inference checker will be used
    /// to verify the propagations done by this propagator are correct.
    ///
    /// See [`InferenceChecker`] for more information.
    fn add_inference_checkers(&self, _checkers: InferenceCheckers<'_>) {}

    /// Create the propagator instance from `Self`.
    ///
    /// Alongside the propagator instance, this returns the events for which the propagator should
    /// be enqueued.
    fn create(
        self,
        context: PropagatorConstructorContext,
    ) -> (EventsToRegister, Self::PropagatorImpl);
}

/// Interface used to add [`InferenceChecker`]s to the [`State`].
#[derive(Debug)]
pub struct InferenceCheckers<'state> {
    state: &'state mut State,
    reification_literal: Option<Literal>,
}

impl<'state> InferenceCheckers<'state> {
    #[cfg(feature = "check-propagations")]
    pub(crate) fn new(state: &'state mut State) -> Self {
        InferenceCheckers {
            state,
            reification_literal: None,
        }
    }
}

impl InferenceCheckers<'_> {
    /// Forwards to [`State::add_inference_checker`].
    pub fn add_inference_checker(
        &mut self,
        inference_code: InferenceCode,
        checker: Box<dyn InferenceChecker<Predicate>>,
    ) {
        if let Some(reification_literal) = self.reification_literal {
            let reification_checker = ReifiedChecker {
                inner: checker.into(),
                reification_literal,
            };
            self.state
                .add_inference_checker(inference_code, Box::new(reification_checker));
        } else {
            self.state.add_inference_checker(inference_code, checker);
        }
    }

    pub fn with_reification_literal(&mut self, literal: Literal) {
        self.reification_literal = Some(literal)
    }
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

    /// Add an inference checker for inferences produced by the propagator.
    ///
    /// If the `check-propagations` feature is not enabled, adding an [`InferenceChecker`] will not
    /// do anything.
    pub fn add_inference_checker(
        &mut self,
        inference_code: InferenceCode,
        checker: Box<dyn InferenceChecker<Predicate>>,
    ) {
        self.state.add_inference_checker(inference_code, checker);
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
