use std::fmt::Debug;

use dyn_clone::DynClone;

use crate::AtomicConstraint;
use crate::VariableState;

/// An inference checker tests whether the given state is a conflict under the semantics of an
/// inference rule.
pub trait InferenceChecker<Atomic: AtomicConstraint>: Debug + DynClone {
    /// Whether `state` is a conflict under the rule.
    ///
    /// For the conflict check, all the premises are true in the state and the consequent, if
    /// present, is false. The inference is accepted when a conflict is detected.
    fn check(
        &self,
        state: VariableState<Atomic>,
        premises: &[Atomic],
        consequent: Option<&Atomic>,
    ) -> ConflictCheck;
}

/// The outcome of [`InferenceChecker::check`].
#[must_use]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ConflictCheck {
    /// The state is a conflict under the rule, so the inference is accepted.
    ConflictDetected,
    /// The state is not a conflict under the rule, so the inference is rejected.
    NoConflictDetected,
}

impl ConflictCheck {
    /// [`ConflictCheck::ConflictDetected`] when `is_conflict` holds, and
    /// [`ConflictCheck::NoConflictDetected`] otherwise.
    pub fn detected_if(is_conflict: bool) -> ConflictCheck {
        if is_conflict {
            ConflictCheck::ConflictDetected
        } else {
            ConflictCheck::NoConflictDetected
        }
    }
}

/// Wrapper around `Box<dyn InferenceChecker<Atomic>>` that implements [`Clone`].
#[derive(Debug)]
pub struct BoxedChecker<Atomic: AtomicConstraint>(Box<dyn InferenceChecker<Atomic>>);

impl<Atomic: AtomicConstraint> Clone for BoxedChecker<Atomic> {
    fn clone(&self) -> Self {
        BoxedChecker(dyn_clone::clone_box(&*self.0))
    }
}

impl<Atomic: AtomicConstraint> BoxedChecker<Atomic> {
    pub fn new(value: Box<dyn InferenceChecker<Atomic>>) -> Self {
        BoxedChecker(value)
    }
}

impl<Atomic: AtomicConstraint> BoxedChecker<Atomic> {
    /// See [`InferenceChecker::check`].
    pub fn check(
        &self,
        variable_state: VariableState<Atomic>,
        premises: &[Atomic],
        consequent: Option<&Atomic>,
    ) -> ConflictCheck {
        self.0.check(variable_state, premises, consequent)
    }
}
