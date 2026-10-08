//! Exposes a common interface used to check inferences.
//!
//! The main exposed type is the [`ConflictChecker`], which can be implemented to verify whether
//! inferences are sound w.r.t. an inference rule.

mod atomic_constraint;
mod conflict_checker;
mod deduction_checker;
mod domain_view;
mod int_ext;
mod union;
mod variable;
mod variable_state;

pub use atomic_constraint::*;
pub use conflict_checker::*;
pub use deduction_checker::*;
pub use domain_view::*;
pub use int_ext::*;
pub use union::*;
pub use variable::*;
pub use variable_state::*;
