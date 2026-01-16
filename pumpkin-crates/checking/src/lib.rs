//! Exposes a common interface used to check inferences.
//!
//! The main exposed type is the [`InferenceChecker`], which can be implemented to verify whether
//! inferences are sound w.r.t. an inference rule.

mod atomic_constraint;
mod i32_ext;
mod variable;
mod variable_state;

use std::fmt::Debug;

pub use atomic_constraint::*;
use dyn_clone::DynClone;
pub use i32_ext::*;
pub use variable::*;
pub use variable_state::*;

/// An inference checker tests whether the given state is a conflict under the sematics of an
/// inference rule.
pub trait InferenceChecker<Atomic: AtomicConstraint>: Debug + DynClone {
    /// Returns `true` if `state` is a conflict, and `false` if not.
    fn check(&self, state: VariableState<Atomic>) -> bool;
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
    pub fn check(&self, variable_state: VariableState<Atomic>) -> bool {
        self.0.check(variable_state)
    }
}
