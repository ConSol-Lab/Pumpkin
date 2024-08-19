use super::Constraint;
use crate::propagators::element::ElementConstructor;
use crate::variables::IntegerVariable;

/// Creates the [element](https://sofdem.github.io/gccat/gccat/Celement.html) [`Constraint`] which states that `array[index] = rhs`.
pub fn element<ElementVar: IntegerVariable + 'static>(
    index: impl IntegerVariable + 'static,
    array: impl Into<Box<[ElementVar]>>,
    rhs: impl IntegerVariable + 'static,
) -> impl Constraint {
    ElementConstructor {
        index,
        array: array.into(),
        rhs,
    }
}
