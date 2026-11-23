//! Support for half-reified propagators, i.e. propagators for `r -> C` where `r` is a literal.
//!
//! Following Chuffed, half-reification is not implemented by a generic wrapper around a
//! propagator. Instead, every reifiable propagator is parametrised by a [`Reification`] type and
//! handles the reification literal itself:
//! - [`NotReified`] is used for the plain constraint `C`. All reification checks are resolved at
//!   compile time, so there is no overhead.
//! - [`ImpliedBy`] is used for the half-reified constraint `r -> C`.
//!
//! A reifiable propagator is expected to:
//! 1. Register the reification literal with [`Reification::register`] under
//!    [`REIFICATION_LOCAL_ID`].
//! 2. Wrap its inference checkers with [`RuntimeCheckers::reified`].
//! 3. Start [`Propagator::propagate`] (and [`Propagator::propagate_from_scratch`]) with
//!    [`Reification::should_propagate`], and pass the result of propagation through
//!    [`Reification::add_to_conflict`].
//! 4. Add the reification literal to every lazy explanation with
//!    [`Reification::add_to_explanation`].
//! 5. Filter the enqueue decision in [`Propagator::notify`] with
//!    [`Reification::filter_enqueue_decision`].
use std::fmt::Debug;

use pumpkin_checking::AtomicConstraint;
use pumpkin_checking::BoxedChecker;
use pumpkin_checking::CheckerVariable;
use pumpkin_checking::InferenceChecker;

use crate::engine::PropagationStatusCP;
use crate::engine::PropagatorConflict;
use crate::predicates::Predicate;
use crate::propagation::DomainEvents;
use crate::propagation::Domains;
use crate::propagation::EnqueueDecision;
use crate::propagation::EventsToRegister;
use crate::propagation::LocalId;
use crate::propagation::NotificationContext;
use crate::propagation::PropagationContext;
#[cfg(doc)]
use crate::propagation::Propagator;
use crate::propagation::ReadDomains;
#[cfg(doc)]
use crate::propagation::RuntimeCheckers;
use crate::state::Conflict;
use crate::variables::Literal;

/// The [`LocalId`] under which a reifiable propagator registers its reification literal.
///
/// It is reserved, so propagators should not use it for any of their other variables.
pub const REIFICATION_LOCAL_ID: LocalId = LocalId::from(u32::MAX);

/// Determines whether a propagator is half-reified.
///
/// This plays the role of the `R` template parameter in Chuffed's propagators.
pub trait Reification: Clone + Copy + Debug + 'static {
    /// The reification literal `r` in `r -> C`, or [`None`] if the propagator is not reified.
    fn literal(&self) -> Option<Literal>;

    /// Registers the reification literal (if any) under [`REIFICATION_LOCAL_ID`].
    fn register(&self, registration: &mut EventsToRegister) {
        if let Some(literal) = self.literal() {
            registration.add(&literal, DomainEvents::BOUNDS, REIFICATION_LOCAL_ID);
        }
    }

    /// Decides whether the propagator should propagate `C`.
    ///
    /// - If `r` is true, the reification literal is added to all eager reasons posted through the
    ///   context, and `true` is returned.
    /// - If `r` is false, the constraint is inactive and `false` is returned.
    /// - If `r` is unassigned, `r` is propagated to false if `detect_inconsistency` finds that `C`
    ///   cannot be satisfied, and `false` is returned.
    #[inline(always)]
    fn should_propagate(
        &self,
        context: &mut PropagationContext,
        detect_inconsistency: impl FnOnce(Domains) -> Option<PropagatorConflict>,
    ) -> Result<bool, Conflict> {
        let Some(literal) = self.literal() else {
            return Ok(true);
        };

        match context.evaluate_literal(literal) {
            Some(true) => {
                context.with_reification(literal);
                Ok(true)
            }
            Some(false) => Ok(false),
            None => {
                if let Some(conflict) = detect_inconsistency(context.domains()) {
                    context.post(
                        literal.get_false_predicate(),
                        (conflict.conjunction, &conflict.inference_code),
                    )?;
                }
                Ok(false)
            }
        }
    }

    /// Adds the reification literal to a conflict explicitly raised by the propagator.
    ///
    /// Conflicts caused by empty domains already contain the literal, since it is added to every
    /// eager reason posted through the context.
    #[inline(always)]
    fn add_to_conflict(&self, mut status: PropagationStatusCP) -> PropagationStatusCP {
        if let Some(literal) = self.literal()
            && let Err(Conflict::Propagator(ref mut conflict)) = status
        {
            conflict.conjunction.push(literal.get_true_predicate());
        }
        status
    }

    /// Adds the reification literal to a lazy explanation.
    #[inline(always)]
    fn add_to_explanation(&self, explanation: &mut Vec<Predicate>) {
        if let Some(literal) = self.literal() {
            explanation.push(literal.get_true_predicate());
        }
    }

    /// Filters the [`EnqueueDecision`] of the propagator for an event on one of its own
    /// variables.
    ///
    /// When `r` is unassigned, the propagator is only enqueued if it can propagate `r` to false.
    /// When `r` is false, it is never enqueued.
    #[inline(always)]
    fn filter_enqueue_decision(
        &self,
        context: &mut NotificationContext,
        decision: EnqueueDecision,
        detect_inconsistency: impl FnOnce(Domains) -> Option<PropagatorConflict>,
    ) -> EnqueueDecision {
        let Some(literal) = self.literal() else {
            return decision;
        };

        if decision == EnqueueDecision::Skip {
            return EnqueueDecision::Skip;
        }

        match context.evaluate_literal(literal) {
            Some(true) => EnqueueDecision::Enqueue,
            Some(false) => EnqueueDecision::Skip,
            None if detect_inconsistency(context.domains()).is_some() => EnqueueDecision::Enqueue,
            None => EnqueueDecision::Skip,
        }
    }
}

/// The propagator is not reified; it propagates `C`.
#[derive(Clone, Copy, Debug, Default)]
pub struct NotReified;

impl Reification for NotReified {
    #[inline(always)]
    fn literal(&self) -> Option<Literal> {
        None
    }
}

/// The propagator is half-reified; it propagates `r -> C`.
#[derive(Clone, Copy, Debug)]
pub struct ImpliedBy(pub Literal);

impl Reification for ImpliedBy {
    #[inline(always)]
    fn literal(&self) -> Option<Literal> {
        Some(self.0)
    }
}

/// An inference checker for an inference made by a half-reified propagator.
#[derive(Debug, Clone)]
pub struct ReifiedChecker<Atomic: AtomicConstraint, Var> {
    pub inner: BoxedChecker<Atomic>,
    pub reification_literal: Var,
}

impl<Atomic: AtomicConstraint + Clone, Var: CheckerVariable<Atomic>> InferenceChecker<Atomic>
    for ReifiedChecker<Atomic, Var>
{
    fn check(
        &self,
        state: pumpkin_checking::VariableState<Atomic>,
        premises: &[Atomic],
        consequent: Option<&Atomic>,
    ) -> bool {
        if self.reification_literal.induced_domain_contains(&state, 0) {
            return false;
        }

        if let Some(consequent) = consequent
            && self
                .reification_literal
                .does_atomic_constrain_self(consequent)
        {
            self.inner.check(state, premises, None)
        } else {
            self.inner.check(state, premises, consequent)
        }
    }
}
