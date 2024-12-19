use super::Constraint;
use crate::propagators::inverse::InversePropagator;
use crate::variables::IntegerVariable;

/// Creates the invers [`Constraint`] which states that `lhs[i] = j && rhs[j] = i`.
pub fn inverse<ElementVar: IntegerVariable + 'static>(
    lhs: impl IntoIterator<Item = ElementVar>,
    rhs: impl IntoIterator<Item = ElementVar>,
) -> impl Constraint {
    InversePropagator::new(lhs.into_iter().collect(),rhs.into_iter().collect())
}
