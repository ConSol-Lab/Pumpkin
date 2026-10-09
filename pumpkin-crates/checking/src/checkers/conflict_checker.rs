use std::fmt::Debug;

use dyn_clone::DynClone;

use crate::AtomicConstraint;
use crate::VariableState;

/// A conflict checker tests whether the given state is a conflict under the semantics of an
/// inference rule.
pub trait ConflictChecker<Atomic: AtomicConstraint>: Debug + DynClone {
    /// Whether `state` is a conflict under the rule.
    ///
    /// For the conflict check, all the premises are true in the state and the consequent, if
    /// present, is false. The inference is accepted when a conflict is detected.
    fn check(
        &self,
        state: &VariableState<Atomic>,
        premises: &[Atomic],
        consequent: Option<&Atomic>,
    ) -> ConflictCheck;
}

/// The outcome of [`ConflictChecker::check`].
#[must_use]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ConflictCheck {
    /// The state is a conflict under the rule, so the inference is accepted.
    ConflictDetected,
    /// The state is not a conflict under the rule, so the inference is rejected.
    NoConflictDetected,
}

/// Wrapper around `Box<dyn ConflictChecker<Atomic>>` that implements [`Clone`].
#[derive(Debug)]
pub struct BoxedConflictChecker<Atomic: AtomicConstraint>(Box<dyn ConflictChecker<Atomic>>);

impl<Atomic: AtomicConstraint> Clone for BoxedConflictChecker<Atomic> {
    fn clone(&self) -> Self {
        BoxedConflictChecker(dyn_clone::clone_box(&*self.0))
    }
}

impl<Atomic: AtomicConstraint> BoxedConflictChecker<Atomic> {
    pub fn new(value: Box<dyn ConflictChecker<Atomic>>) -> Self {
        BoxedConflictChecker(value)
    }
}

impl<Atomic: AtomicConstraint> BoxedConflictChecker<Atomic> {
    /// See [`ConflictChecker::check`].
    pub fn check(
        &self,
        state: &VariableState<Atomic>,
        premises: &[Atomic],
        consequent: Option<&Atomic>,
    ) -> ConflictCheck {
        self.0.check(state, premises, consequent)
    }

    /// Check the inference `premises -> consequent` against the rule: the premises together with
    /// the negation of the consequent have to be a conflict.
    ///
    /// Without a consequent, the premises alone have to be a conflict.
    pub fn check_inference(
        &self,
        premises: &[Atomic],
        consequent: Option<&Atomic>,
    ) -> Result<(), RejectedInference<Atomic::Identifier>> {
        let state = VariableState::prepare_for_conflict_check(
            premises.iter().cloned(),
            consequent.cloned(),
        )
        .map_err(RejectedInference::InconsistentPredicates)?;

        match self.check(&state, premises, consequent) {
            ConflictCheck::ConflictDetected => Ok(()),
            ConflictCheck::NoConflictDetected => Err(RejectedInference::NoConflictDetected),
        }
    }
}

/// Why [`BoxedConflictChecker::check_inference`] rejects an inference.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RejectedInference<Identifier> {
    /// The premises and the negated consequent contradict each other on this variable, so they
    /// do not describe a state the rule can be checked in.
    InconsistentPredicates(Identifier),
    /// The rule does not detect a conflict, so the inference is not shown to be sound.
    NoConflictDetected,
}
