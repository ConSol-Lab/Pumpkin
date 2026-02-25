use std::fmt::Debug;

use dyn_clone::DynClone;

use crate::AtomicConstraint;
use crate::VariableState;

/// An inference checker tests whether the given state is a conflict under the sematics of an
/// inference rule.
pub trait InferenceChecker<Atomic: AtomicConstraint>: Debug + DynClone {
    /// Returns `true` if `state` is a conflict, and `false` if not.
    ///
    /// For the conflict check, all the premises are true in the state and the consequent, if
    /// present, if false.
    fn check(
        &self,
        state: VariableState<Atomic>,
        premises: &[Atomic],
        consequent: Option<&Atomic>,
    ) -> bool;
}

/// Wrapper around `Box<dyn InferenceChecker<Atomic>>` that implements [`Clone`].
#[derive(Debug)]
pub struct BoxedChecker<Atomic: AtomicConstraint>(Box<dyn InferenceChecker<Atomic>>);

impl<Atomic: AtomicConstraint> Clone for BoxedChecker<Atomic> {
    fn clone(&self) -> Self {
        BoxedChecker(dyn_clone::clone_box(&*self.0))
    }
}

impl<Atomic: AtomicConstraint> From<Box<dyn InferenceChecker<Atomic>>> for BoxedChecker<Atomic> {
    fn from(value: Box<dyn InferenceChecker<Atomic>>) -> Self {
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
    ) -> bool {
        self.0.check(variable_state, premises, consequent)
    }
}
