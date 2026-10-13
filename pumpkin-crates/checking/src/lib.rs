//! The inference rules of Pumpkin and the checkers that verify them.
//!
//! [`types`] holds the vocabulary the rules are stated in: atomic constraints, variables and the
//! [`VariableState`] they are evaluated against. [`checkers`] holds, per constraint, the
//! [`ConflictChecker`] that verifies an inference is sound. Both the solver and the proof checker
//! use them.

pub mod checkers;
pub mod types;

pub use checkers::BoxedConflictChecker;
pub use checkers::ConflictCheck;
pub use checkers::ConflictChecker;
pub use checkers::IgnoredInference;
pub use checkers::InvalidDeduction;
pub use checkers::SupportingInference;
pub use checkers::verify_deduction;
pub use types::*;
