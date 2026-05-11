use pumpkin_core::constraints::Constraint;
use pumpkin_core::proof::ConstraintTag;
use pumpkin_core::variables::IntegerVariable;
use pumpkin_propagators::all_different::AllDifferentConstructor;

use super::binary_not_equals;

/// Creates the [`Constraint`] that enforces that all the given `variables` are distinct.
pub fn all_different<Var: IntegerVariable + 'static>(
    variables: impl Into<Box<[Var]>>,
    constraint_tag: ConstraintTag,
) -> impl Constraint {
    AllDifferentConstructor {
        x: variables.into(),
        constraint_tag: ConstraintTag,
    }
}
