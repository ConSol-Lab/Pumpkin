//! Contains a bunch of utilities to help write tests for the checker.

use std::num::NonZero;
use std::rc::Rc;

use drcp_format::ConstraintId;
use drcp_format::IntAtomic;

/// Create a constraint ID from the given number.
///
/// Panics if the ID is zero.
pub(crate) fn constraint_id(id: u32) -> ConstraintId {
    NonZero::new(id).expect("constraint id should be non-zero")
}

/// Create an [`drcp_format::IntAtomic`] using a DSL.
///
/// # Example
/// ```
/// atomic!([x >= 5]);
/// atomic!([y != 10]);
///
/// // Use `String` as the identifier instead of `Rc<str>`
/// atomic!([some_var string != 10]);
/// ```
#[macro_export]
macro_rules! atomic {
    (@to_comparison >=) => {
        drcp_format::IntComparison::GreaterEqual
    };
    (@to_comparison <=) => {
        drcp_format::IntComparison::LessEqual
    };
    (@to_comparison ==) => {
        drcp_format::IntComparison::Equal
    };
    (@to_comparison !=) => {
        drcp_format::IntComparison::NotEqual
    };

    ([$name:ident $comp:tt $value:expr]) => {
        drcp_format::IntAtomic {
            name: std::rc::Rc::from(stringify!($name)),
            comparison: atomic!(@to_comparison $comp),
            value: $value,
        }
    };

    ([$name:ident string $comp:tt $value:expr]) => {
        drcp_format::IntAtomic {
            name: String::from(stringify!($name)),
            comparison: atomic!(@to_comparison $comp),
            value: $value,
        }
    };
}

/// Create a [`Fact`] in a DSL.
///
/// # Example
/// ```
/// fact!([x >= 5] & [y <= 10] -> [z == 5]);
/// fact!([x >= 5] & [y <= 10] -> false);
/// ```
#[macro_export]
macro_rules! fact {
    // Case: consequent is an Atomic
    (
        $($prem:tt)&+ -> [$($cons:tt)+]
    ) => {
        Fact {
            premises: vec![
                $( $crate::model::Atomic::IntAtomic(atomic!($prem)) ),+
            ],
            consequent: Some(
                $crate::model::Atomic::IntAtomic(atomic!([$($cons)+]))
            ),
        }
    };

    // Case: consequent is false (i.e., None)
    (
        $($prem:tt)&+ -> false
    ) => {
        Fact {
            premises: vec![
                $( $crate::model::Atomic::IntAtomic(atomic!($prem)) ),+
            ],
            consequent: None,
        }
    };
}

pub(crate) fn deduction(
    id: u32,
    premises: impl Into<Vec<IntAtomic<Rc<str>, i32>>>,
    sequence: impl IntoIterator<Item = u32>,
) -> drcp_format::Deduction<Rc<str>, i32> {
    drcp_format::Deduction {
        constraint_id: constraint_id(id),
        premises: premises.into(),
        sequence: sequence
            .into_iter()
            .map(|id| NonZero::new(id).expect("constraint ids should be non-zero"))
            .collect(),
    }
}
