mod equality;
mod inequality;

pub use equality::*;
pub use inequality::*;
use pumpkin_core::constraints::Constraint;
use pumpkin_core::proof::ConstraintTag;
use pumpkin_core::variables::IntegerVariable;
use pumpkin_propagators::arithmetic::AbsoluteValueArgs;
use pumpkin_propagators::arithmetic::DivisionArgs;
use pumpkin_propagators::arithmetic::IntegerMultiplicationArgs;
use pumpkin_propagators::arithmetic::MaximumArgs;

/// Creates the [`Constraint`] `a + b = c`.
pub fn plus<Var: IntegerVariable + 'static>(
    a: Var,
    b: Var,
    c: Var,
    constraint_tag: ConstraintTag,
) -> impl Constraint {
    equals([a.scaled(1), b.scaled(1), c.scaled(-1)], 0, constraint_tag)
}

/// Creates the [`Constraint`] `a * b = c`.
pub fn times(
    a: impl IntegerVariable + 'static,
    b: impl IntegerVariable + 'static,
    c: impl IntegerVariable + 'static,
    constraint_tag: ConstraintTag,
) -> impl Constraint {
    IntegerMultiplicationArgs {
        a,
        b,
        c,
        constraint_tag,
    }
}

/// Creates the [`Constraint`] `numerator / denominator = rhs`.
///
/// Note that this [`Constraint`] models truncating division (i.e. rounding towards 0).
///
/// The `denominator` should not contain the value 0 in its domain; if this is the case then the
/// solver will panic.
pub fn division(
    numerator: impl IntegerVariable + 'static,
    denominator: impl IntegerVariable + 'static,
    rhs: impl IntegerVariable + 'static,
    constraint_tag: ConstraintTag,
) -> impl Constraint {
    DivisionArgs {
        numerator,
        denominator,
        rhs,
        constraint_tag,
    }
}

/// Creates the [`Constraint`] `|signed| = absolute`.
pub fn absolute(
    signed: impl IntegerVariable + 'static,
    absolute: impl IntegerVariable + 'static,
    constraint_tag: ConstraintTag,
) -> impl Constraint {
    AbsoluteValueArgs {
        signed,
        absolute,
        constraint_tag,
    }
}

/// Creates the [`Constraint`] `max(array) = m`.
pub fn maximum<Var: IntegerVariable + 'static>(
    array: impl IntoIterator<Item = Var>,
    rhs: impl IntegerVariable + 'static,
    constraint_tag: ConstraintTag,
) -> impl Constraint {
    MaximumArgs {
        array: array.into_iter().collect(),
        rhs,
        constraint_tag,
    }
}

/// Creates the [`Constraint`] `min(array) = m`.
pub fn minimum<Var: IntegerVariable + 'static>(
    array: impl IntoIterator<Item = Var>,
    rhs: impl IntegerVariable + 'static,
    constraint_tag: ConstraintTag,
) -> impl Constraint {
    let array = array.into_iter().map(|var| var.scaled(-1));
    maximum(array, rhs.scaled(-1), constraint_tag)
}
