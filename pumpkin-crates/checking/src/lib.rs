//! Exposes a common interface used to check inferences.
//!
//! The main exposed type is the [`InferenceChecker`], which can be implemented to verify whether
//! inferences are sound w.r.t. an inference rule.

mod atomic_constraint;
mod i32_ext;
mod variable;
mod variable_state;

use std::fmt::Debug;

use dyn_clone::DynClone;

pub use atomic_constraint::*;
pub use i32_ext::*;
pub use variable::*;
pub use variable_state::*;

/// An inference checker tests whether the given state is a conflict under the sematics of an
/// inference rule.
pub trait InferenceChecker<Atomic: AtomicConstraint>: Debug + DynClone {
    /// Returns `true` if `state` is a conflict, and `false` if not.
    fn check(&self, state: VariableState<Atomic>) -> bool;
}
