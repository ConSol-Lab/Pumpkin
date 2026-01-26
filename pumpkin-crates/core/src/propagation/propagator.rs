use downcast_rs::Downcast;
use downcast_rs::impl_downcast;
use dyn_clone::DynClone;
use dyn_clone::clone_trait_object;

use super::Domains;
use super::ExplanationContext;
use super::PropagationContext;
use super::contexts::NotificationContext;
use crate::basic_types::PredicateId;
use crate::basic_types::PropagationStatusCP;
use crate::basic_types::PropagatorConflict;
#[cfg(doc)]
use crate::create_statistics_struct;
#[cfg(doc)]
use crate::engine::ConstraintSatisfactionSolver;
use crate::engine::notifications::OpaqueDomainEvent;
use crate::predicates::Predicate;
#[cfg(doc)]
use crate::propagation::DomainEvent;
#[cfg(doc)]
use crate::propagation::PropagatorConstructor;
#[cfg(doc)]
use crate::propagation::PropagatorConstructorContext;
#[cfg(doc)]
use crate::propagation::ReadDomains;
use crate::propagation::local_id::LocalId;
#[cfg(doc)]
use crate::pumpkin_asserts::PUMPKIN_ASSERT_ADVANCED;
#[cfg(doc)]
use crate::pumpkin_asserts::PUMPKIN_ASSERT_EXTREME;
#[cfg(doc)]
use crate::state::Conflict;
use crate::statistics::statistic_logger::StatisticLogger;

// We need to use this to cast from `Box<dyn Propagator>` to `NogoodPropagator`; rust inherently
// does not allow downcasting from the trait definition to its concrete type.
impl_downcast!(Propagator);

// To allow the State object to be cloneable, we need to allow `Box<dyn Propagator>` to be cloned.
clone_trait_object!(Propagator);

/// A propagator removes values from domains which will never be in any solution, or raises
/// explicit conflicts.
///
/// The only required functions are [`Propagator::name`],
///  and [`Propagator::propagate_from_scratch`]; all other
/// functions have default implementations. For initial development, the required functions are
/// enough, but a more mature implementation considers all functions in most cases.
///
/// See the [`crate::propagation`] documentation for more details.
pub trait Propagator: Downcast + DynClone {
    /// Return the name of the propagator.
    ///
    /// This is a convenience method that is used for printing.
    fn name(&self) -> &str;

    /// Performs propagation from scratch (i.e., without relying on updating
    /// internal data structures, as opposed to [`Propagator::propagate`]).
    ///
    /// The main aims of this method are to remove values from the domains of variables (using
    /// [`PropagationContext::post`]) which cannot be part of any solution given the current
    /// domains and to detect conflicts.
    ///
    /// In case no conflict has been detected this function should
    /// return [`Result::Ok`], otherwise it should return a [`Result::Err`] with a [`Conflict`]
    /// which contains the reason for the failure; either because a propagation caused an
    /// an empty domain ([`Conflict::EmptyDomain`] as a result of [`PropagationContext::post`]) or
    /// because the logic of the propagator found the current state to be inconsistent
    /// ([`Conflict::Propagator`] ).
    ///
    /// It is usually best to implement this propagation method in the simplest
    /// but correct way. When this crate is compiled with the `debug-checks` feature, this method
    /// will be called to double check the reasons for failures and propagations that have been
    /// reported by this propagator.
    ///
    /// Propagators are not required to propagate until a fixed point. It will be called again by
    /// the solver until no further propagations happen.
    fn propagate_from_scratch(&self, context: PropagationContext) -> PropagationStatusCP;

    /// Performs propagation with state (i.e., with being able to mutate internal data structures,
    /// as opposed to [`Propagator::propagate_from_scratch`]).
    ///
    /// The main aims of this method are to remove values from the domains of variables (using
    /// [`PropagationContext::post`]) which cannot be part of any solution given the current
    /// domains and to detect conflicts.
    ///
    /// In case no conflict has been detected this function should
    /// return [`Result::Ok`], otherwise it should return a [`Result::Err`] with a [`Conflict`]
    /// which contains the reason for the failure; either because a propagation caused an
    /// an empty domain ([`Conflict::EmptyDomain`] as a result of [`PropagationContext::post`]) or
    /// because the logic of the propagator found the current state to be inconsistent
    /// ([`Conflict::Propagator`] ).
    ///
    /// Propagators are not required to propagate until a fixed point. It will be called
    /// again by the solver until no further propagations happen.
    ///
    /// By default, this function calls [`Propagator::propagate_from_scratch`].
    fn propagate(&mut self, context: PropagationContext) -> PropagationStatusCP {
        self.propagate_from_scratch(context)
    }

    /// Returns whether the propagator should be enqueued for propagation when a [`DomainEvent`]
    /// happens to one of the variables the propagator is subscribed to (as registered during
    /// creation with [`PropagatorConstructor`] using [`PropagatorConstructorContext::register`]).
    ///
    /// This can be used to incrementally maintain data structures or perform propagations, and
    /// should only be used for computationally cheap logic. Expensive computation should be
    /// performed in the [`Propagator::propagate`] method.
    ///
    /// By default the propagator is always enqueued for every event it is subscribed to. Not all
    /// propagators will benefit from implementing this, so it is not required to do so.
    fn notify(
        &mut self,
        _context: NotificationContext,
        _local_id: LocalId,
        _event: OpaqueDomainEvent,
    ) -> EnqueueDecision {
        EnqueueDecision::Enqueue
    }

    /// This function is called when the effect of a [`DomainEvent`] is undone during backtracking
    /// of one of the variables the propagator is subscribed to (as registered during creation with
    /// [`PropagatorConstructor`] using [`PropagatorConstructorContext::register_backtrack`]).
    ///
    /// This can be used to incrementally maintain data structures or perform propagations, and
    /// should only be used for computationally cheap logic. Expensive computation should be
    /// performed in the [`Propagator::propagate`] method.
    ///
    /// *Note*: This method is only called for [`DomainEvent`]s for which [`Propagator::notify`]
    /// was called. This means that if the propagator itself made a change, but was not notified of
    /// it (e.g., due to a conflict being detected), then this method will also not be called for
    /// that [`DomainEvent`].
    ///
    /// By default the propagator does nothing when this method is called. Not all propagators will
    /// benefit from implementing this, so it is not required to do so.
    fn notify_backtrack(
        &mut self,
        _context: Domains,
        _local_id: LocalId,
        _event: OpaqueDomainEvent,
    ) {
    }

    /// Returns whether the propagator should be enqueued for propagation when a [`Predicate`] (with
    /// corresponding [`PredicateId`]) which the propagator is subscribed to (as registered either
    /// during using [`PropagationContext::register_predicate`] or during creation with
    /// [`PropagatorConstructor`] using [`PropagatorConstructorContext::register_predicate`]).
    ///
    /// By default, the propagator will be enqueued.
    fn notify_predicate_id_satisfied(
        &mut self,
        _context: NotificationContext,
        _predicate_id: PredicateId,
    ) -> EnqueueDecision {
        EnqueueDecision::Enqueue
    }

    /// Called after backtracking, allowing the propagator to
    /// update its internal data structures given the new variable domains.
    ///
    /// By default this function does nothing.
    fn synchronise(&mut self, _domains: Domains) {}

    /// Returns the [`Priority`] of the propagator, used for determining the order in which
    /// propagators are called.
    ///
    /// See [`Priority`] documentation for more explanation.
    ///
    /// By default the priority is set to [`Priority::VeryLow`]. It is expected that
    /// propagator implementations would set this value to some appropriate value.
    fn priority(&self) -> Priority {
        Priority::VeryLow
    }

    /// A function which returns [`Some`] with a [`PropagatorConflict`] when this propagator can
    /// detect an inconsistency (and [`None`] otherwise).
    ///
    /// By implementing this function, if the propagator is reified, it can propagate the
    /// reification literal based on the detected inconsistency. Yet, an implementation is not
    /// needed for correctness, as [`Propagator::propagate`] should still check for
    /// inconsistency as well.
    fn detect_inconsistency(&self, _domains: Domains) -> Option<PropagatorConflict> {
        None
    }

    /// Hook which is called when a propagated [`Predicate`] should be explained using a lazy
    /// reason.
    ///
    /// The code which was attached to the propagation is given, as
    /// well as a context object which defines what can be inspected from the solver to build the
    /// explanation.
    ///
    /// *Note:* The context which is provided contains the _current_ state (i.e. the state when the
    /// explanation is generated); the bounds at the time of the propagation can be retrieved using
    /// methods such as [`ReadDomains::lower_bound_at_trail_position`] in combination
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
pub enum EnqueueDecision {
    /// The propagator should be enqueued.
    Enqueue,
    /// The propagator should not be enqueued.
    Skip,
}

/// The priority of a propagator, used for determining the order in which propagators will be
/// called.
///
/// Propagators with high priority are propagated before propagators with low(er) priority. If two
/// propagators have the same priority, then the order in which they are propagated is unspecified.
///
/// Typically, propagators with low computational complexity should be assigned a high
/// priority (i.e., should be propagated before computationally expensive propagators).
#[derive(Default, Debug, Clone, Copy, Hash, PartialEq, Eq)]
#[repr(u8)]
pub enum Priority {
    High = 0,
    Medium = 1,
    Low = 2,
    #[default]
    VeryLow = 3,
}

impl PartialOrd for Priority {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        ((*self) as u8).partial_cmp(&((*other) as u8))
    }
}
