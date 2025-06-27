use super::Constraint;
use crate::proof::ConstraintTag;
use crate::propagators::element::ElementArgs;
use crate::variables::IntegerVariable;

/// Creates the [element](https://sofdem.github.io/gccat/gccat/Celement.html) [`Constraint`] which states that `array[index] = rhs`.
pub fn element<ElementVar: IntegerVariable + 'static>(
    index: impl IntegerVariable + 'static,
    array: impl IntoIterator<Item = ElementVar>,
    rhs: impl IntegerVariable + 'static,
    constraint_tag: ConstraintTag,
) -> impl Constraint {
    ElementArgs {
        array: array.into_iter().collect(),
        index,
        rhs,
        constraint_tag,
    }
}
