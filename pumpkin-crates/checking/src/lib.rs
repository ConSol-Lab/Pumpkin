//! Exposes a common interface used to check inferences.
//!
//! The main exposed type is the [`InferenceChecker`], which can be implemented to verify whether
//! inferences are sound w.r.t. an inference rule.

mod atomic_constraint;
mod inference_checker;
mod int_ext;
mod union;
mod variable;
mod variable_state;

pub use atomic_constraint::*;
pub use inference_checker::*;
pub use int_ext::*;
pub use union::*;
pub use variable::*;
pub use variable_state::*;
