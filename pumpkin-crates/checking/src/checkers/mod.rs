//! The checkers of the inference rules.
//!
//! Each constraint has a folder holding the checker of its rule: the type in `mod.rs`, the
//! [`InferenceChecker`] implementation in `inference.rs` and, where the rule has one, the
//! [`RetentionChecker`] implementation in `retention.rs`.

mod deduction_checker;
mod inference_checker;
mod retention_checker;

mod absolute_value;
mod binary_equals;
mod binary_not_equals;
mod cumulative;
mod disjunctive;
mod element;
mod hypercube_linear;
mod integer_division;
mod integer_multiplication;
mod linear_less_or_equal;
mod linear_not_equal;
mod maximum;
mod nogood;
mod reified;

pub use absolute_value::*;
pub use binary_equals::*;
pub use binary_not_equals::*;
pub use cumulative::*;
pub use deduction_checker::*;
pub use disjunctive::*;
pub use element::*;
pub use hypercube_linear::*;
pub use inference_checker::*;
pub use integer_division::*;
pub use integer_multiplication::*;
pub use linear_less_or_equal::*;
pub use linear_not_equal::*;
pub use maximum::*;
pub use nogood::*;
pub use reified::*;
pub use retention_checker::*;

/// The state in which the given atomic constraints hold, for tests of the checkers.
#[cfg(test)]
pub(crate) fn test_state(
    atomics: impl IntoIterator<Item = crate::TestAtomic>,
) -> crate::VariableState<crate::TestAtomic> {
    crate::VariableState::prepare_for_conflict_check(atomics, None)
        .expect("the atomic constraints are consistent")
}
