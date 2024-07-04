use crate::engine::propagation::propagator::Propagator;
use crate::engine::propagation::propagator_constructor_context::PropagatorConstructorContext;
#[cfg(doc)]
use crate::engine::propagation::propagator_variable::PropagatorVariable;

/// A CP propagator constructor turns an argument struct into an implementation of
/// [`Propagator`]. Note that each propagator has a corresponding constructor.
///
/// The constructor is used as a communcation point between the solver and propagators.
/// Propagators do not have direct access to the internal variable representation
/// of the solver, and instead use [`PropagatorVariable`].
/// [`PropagatorVariable`]s are obtained through the [`PropagatorConstructorContext`]
/// that is passed to the constructor when the propagator is added the solver.
pub trait PropagatorConstructor {
    /// The propagator to construct.
    type Propagator: Propagator;

    /// Creates the corresponding propagator and subscribed it to variables and events.
    /// The subscription is done through [`PropagatorConstructorContext::register()`].
    fn create(self, context: PropagatorConstructorContext<'_>) -> Self::Propagator;

    /// A handy boxed constructor function.
    /// Invokes [`PropagatorConstructor::create()`] and returns the propagator in a [`Box`].
    fn create_boxed(self, context: PropagatorConstructorContext<'_>) -> Box<dyn Propagator>
    where
        Self: Sized,
        Self::Propagator: 'static,
    {
        Box::new(self.create(context))
    }
}
