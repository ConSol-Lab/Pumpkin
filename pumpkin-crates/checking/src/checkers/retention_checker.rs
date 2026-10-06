use std::fmt::Debug;

use dyn_clone::DynClone;

use crate::AtomicConstraint;
use crate::DomainView;

/// Verifies that a propagator has nothing left to propagate.
///
/// The checker reads the domains of the variables in its scope, and each of them is bounded. The
/// check succeeds when no inference of the rule applies in those domains: giving any variable any
/// value of its domain does not let the rule report a conflict.
pub trait RetentionChecker<Atomic: AtomicConstraint>: Debug + DynClone {
    /// Whether some inference of the rule still applies in `domains`.
    fn check_retention(&self, domains: &dyn DomainView<Atomic>) -> RetentionCheck;
}

/// The outcome of [`RetentionChecker::check_retention`].
#[must_use]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RetentionCheck {
    /// No inference of the rule applies, so the propagator reached its fixpoint.
    NothingToPropagate,
    /// Some inference of the rule still applies, which the propagator missed.
    PropagationMissed,
}

/// Wrapper around `Box<dyn RetentionChecker<Atomic>>` that implements [`Clone`].
#[derive(Debug)]
pub struct BoxedRetentionChecker<Atomic: AtomicConstraint>(Box<dyn RetentionChecker<Atomic>>);

impl<Atomic: AtomicConstraint> Clone for BoxedRetentionChecker<Atomic> {
    fn clone(&self) -> Self {
        BoxedRetentionChecker(dyn_clone::clone_box(&*self.0))
    }
}

impl<Atomic: AtomicConstraint> BoxedRetentionChecker<Atomic> {
    pub fn new(checker: impl RetentionChecker<Atomic> + 'static) -> Self {
        BoxedRetentionChecker(Box::new(checker))
    }

    /// See [`RetentionChecker::check_retention`].
    pub fn check_retention(&self, domains: &dyn DomainView<Atomic>) -> RetentionCheck {
        self.0.check_retention(domains)
    }
}
