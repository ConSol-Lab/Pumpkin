mod equality;
mod inequality;

pub use equality::*;
pub use inequality::*;

use super::Constraint;
use crate::propagators::absolute_value::AbsoluteValueConstructor;
use crate::propagators::division::DivisionConstructor;
use crate::propagators::integer_multiplication::IntegerMultiplicationConstructor;
use crate::propagators::maximum::MaximumConstructor;
use crate::variables::IntegerVariable;

/// Creates the [`Constraint`] `a + b = c`.
pub fn plus<Var: IntegerVariable + 'static>(a: Var, b: Var, c: Var) -> impl Constraint {
    equals([a.scaled(1), b.scaled(1), c.scaled(-1)], 0)
}

/// Creates the [`Constraint`] `a * b = c`.
pub fn times(
    a: impl IntegerVariable + 'static,
    b: impl IntegerVariable + 'static,
    c: impl IntegerVariable + 'static,
) -> impl Constraint {
    IntegerMultiplicationConstructor { a, b, c }
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
) -> impl Constraint {
    DivisionConstructor {
        numerator,
        denominator,
        rhs,
    }
}

/// Creates the [`Constraint`] `|signed| = absolute`.
pub fn absolute(
    signed: impl IntegerVariable + 'static,
    absolute: impl IntegerVariable + 'static,
) -> impl Constraint {
    AbsoluteValueConstructor { signed, absolute }
}

/// Creates the [`Constraint`] `max(array) = m`.
pub fn maximum<Var: IntegerVariable + 'static>(
    array: impl Into<Box<[Var]>>,
    rhs: impl IntegerVariable + 'static,
) -> impl Constraint {
    MaximumConstructor {
        array: array.into(),
        rhs,
    }
}

/// Creates the [`Constraint`] `min(array) = m`.
pub fn minimum<Var: IntegerVariable + 'static>(
    array: impl IntoIterator<Item = Var>,
    rhs: impl IntegerVariable + 'static,
) -> impl Constraint {
    let array = array
        .into_iter()
        .map(|var| var.scaled(-1))
        .collect::<Box<_>>();
    maximum(array, rhs.scaled(-1))
}
