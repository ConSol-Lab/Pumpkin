use downcast_rs::impl_downcast;
use downcast_rs::Downcast;

use super::contexts::PropagationContextWithTrailedValues;
use super::ExplanationContext;
use super::PropagationContext;
use super::PropagationContextMut;
#[cfg(doc)]
use crate::basic_types::Inconsistency;
use crate::basic_types::PredicateId;
use crate::basic_types::PropagationStatusCP;
use crate::basic_types::PropagatorConflict;
#[cfg(doc)]
use crate::create_statistics_struct;
use crate::engine::notifications::OpaqueDomainEvent;
use crate::engine::propagation::local_id::LocalId;
#[cfg(doc)]
use crate::engine::ConstraintSatisfactionSolver;
use crate::predicates::Predicate;
#[cfg(doc)]
use crate::pumpkin_asserts::PUMPKIN_ASSERT_ADVANCED;
#[cfg(doc)]
use crate::pumpkin_asserts::PUMPKIN_ASSERT_EXTREME;
use crate::statistics::statistic_logger::StatisticLogger;

// We need to use this to cast from `Box<dyn Propagator>` to `NogoodPropagator`; rust inherently
// does not allow downcasting from the trait definition to its concrete type.
impl_downcast!(Propagator);

/// All propagators implement the [`Propagator`] trait, which defines the main propagator logic with
/// regards to propagation, detecting conflicts, and providing explanations.
///
/// The only required functions are [`Propagator::name`],
/// [`Propagator::initialise_at_root`], and [`Propagator::debug_propagate_from_scratch`]; all other
/// functions have default implementations. For initial development, the required functions are
/// enough, but a more mature implementation considers all functions in most cases.
///
/// See the [`crate::engine::cp::propagation`] documentation for more details.
pub(crate) trait Propagator: Downcast {
    /// Return the name of the propagator, this is a convenience method that is used for printing.
    fn name(&self) -> &str;

    /// A propagation method that is used to help debugging.
    ///
    /// This method propagates without relying on internal data structures, hence the immutable
    /// &self parameter. It is usually best to implement this propagation method in the simplest
    /// but correct way. When the assert level is set to [`PUMPKIN_ASSERT_ADVANCED`] or
    /// [`PUMPKIN_ASSERT_EXTREME`] (see [`crate::pumpkin_asserts`]) this method will be called
    /// to double check the reasons for failures and propagations that have been reported by
    /// this propagator.
    ///
    /// Propagators are not required to propagate until a fixed point. It will be called again by
    /// the solver until no further propagations happen.
    fn debug_propagate_from_scratch(&self, context: PropagationContextMut) -> PropagationStatusCP;

    /// Propagate method that will be called during search (e.g. in
    /// [`ConstraintSatisfactionSolver::solve`]).
    ///
    /// This method extends the current partial
    /// assignments with inferred domain changes found by the
    /// [`Propagator`]. In case no conflict has been detected it should return
    /// [`Result::Ok`], otherwise it should return a [`Result::Err`] with an [`Inconsistency`] which
    /// contains the reason for the failure; either because a propagation caused an
    /// an empty domain ([`Inconsistency::EmptyDomain`]) or because the logic of the propagator
    /// found the current state to be inconsistent ([`Inconsistency::Conflict`]).
    ///
    /// Note that the failure (explanation) is given as a conjunction of predicates that lead to the
    /// failure
    ///
    /// Propagators are not required to propagate until a fixed point. It will be called
    /// again by the solver until no further propagations happen.
    ///
    /// By default, this function calls [`Propagator::debug_propagate_from_scratch`].
    fn propagate(&mut self, context: PropagationContextMut) -> PropagationStatusCP {
        self.debug_propagate_from_scratch(context)
    }

    /// Called when an event happens to one of the variables the propagator is subscribed to. It
    /// indicates whether the provided event should cause the propagator to be enqueued.
    ///
    /// This can be used to incrementally maintain data structures or perform propagations, and
    /// should only be used for computationally cheap logic. Expensive computation should be
    /// performed in the [`Propagator::propagate()`] method.
    ///
    /// By default the propagator is always enqueued for every event. Not all propagators will
    /// benefit from implementing this, so it is not required to do so.
    ///
    /// Note that the variables and events to which the propagator is subscribed to are determined
    /// upon propagator initialisation via [`Propagator::initialise_at_root`] by calling
    /// [`PropagatorInitialisationContext::register()`].
    fn notify(
        &mut self,
        _context: PropagationContextWithTrailedValues,
        _local_id: LocalId,
        _event: OpaqueDomainEvent,
    ) -> EnqueueDecision {
        EnqueueDecision::Enqueue
    }

    /// Called when an event happens to one of the variables the propagator is subscribed to. This
    /// method is called during backtrack when the domain of a variable has been undone.
    ///
    /// This can be used to incrementally maintain data structures or perform propagations, and
    /// should only be used for computationally cheap logic. Expensive computation should be
    /// performed in the [`Propagator::propagate`] method.
    ///
    /// By default the propagator does nothing when this method is called. Not all propagators will
    /// benefit from implementing this, so it is not required to do so.
    ///
    /// Note that the variables and events to which the propagator is subscribed to are determined
    /// upon propagator initialisation via [`Propagator::initialise_at_root`] by calling
    /// [`PropagatorInitialisationContext::register()`].
    fn notify_backtrack(
        &mut self,
        _context: PropagationContext,
        _local_id: LocalId,
        _event: OpaqueDomainEvent,
    ) {
    }

    /// Called when a [`PredicateId`] has been satisfied.
    ///
    /// By default, the propagator does nothing when this method is called.
    fn notify_predicate_id_satisfied(&mut self, _predicate_id: PredicateId) {}

    /// Called when a [`PredicateId`] has been falsified.
    ///
    /// By default, the propagator does nothing when this method is called.
    #[allow(dead_code, reason = "Will be part of the public API")]
    fn notify_predicate_id_falsified(&mut self, _predicate_id: PredicateId) {}

    /// Called each time the [`ConstraintSatisfactionSolver`] backtracks, the propagator can then
    /// update its internal data structures given the new variable domains.
    ///
    /// By default this function does nothing.
    fn synchronise(&mut self, _context: PropagationContext) {}

    /// Returns the priority of the propagator represented as an integer. Lower values mean higher
    /// priority and the priority determines the order in which propagators will be asked to
    /// propagate. It is custom for simpler propagators to have lower priority values.
    ///
    /// By default the priority is set to 3. It is expected that propagator implementations would
    /// set this value to some appropriate value.
    fn priority(&self) -> u32 {
        // setting an arbitrary priority by default
        3
    }

    /// A check whether this propagator can detect an inconsistency.
    ///
    /// By implementing this function, if the propagator is reified, it can propagate the
    /// reification literal based on the detected inconsistency. Yet, an implementation is not
    /// needed for correctness, as [`Propagator::propagate`] should still check for
    /// inconsistency as well.
    fn detect_inconsistency(
        &self,
        _context: PropagationContextWithTrailedValues,
    ) -> Option<PropagatorConflict> {
        None
    }

    /// Hook which is called when a propagation was done with a lazy reason.
    ///
    /// The code which was attached to the propagation through [`Reason::DynamicLazy`] is given, as
    /// well as a context object which defines what can be inspected from the solver to build the
    /// explanation.
    ///
    /// *Note:* The context which is provided contains the _current_ state (i.e. the state when the
    /// explanation is generated); the bounds at the time of the propagation can be retrieved using
    /// methods such as [`ExplanationContext::get_lower_bound_at_trail_position`] in combination
    /// with [`ExplanationContext::get_trail_position`].
    fn lazy_explanation(&mut self, _code: u64, _context: ExplanationContext) -> &[Predicate] {
        panic!(
            "{}",
            format!(
                "Propagator {} does not support lazy explanations.",
                self.name()
            )
        );
    }
    /// Logs statistics of the propagator using the provided [`StatisticLogger`].
    ///
    /// It is recommended to create a struct through the [`create_statistics_struct!`] macro!
    fn log_statistics(&self, _statistic_logger: StatisticLogger) {}
}

/// Indicator of what to do when a propagator is notified.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum EnqueueDecision {
    /// The propagator should be enqueued.
    Enqueue,
    /// The propagator should not be enqueued.
    Skip,
}
