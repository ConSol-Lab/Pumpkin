use implementation::propagators::all_different::AllDifferentConstructor;
use pumpkin_core::constraints::Constraint;
use pumpkin_core::proof::ConstraintTag;
use pumpkin_core::variables::IntegerVariable;

/// Creates the [`Constraint`] that enforces that all the given `variables` are distinct.
pub fn all_different<Var: IntegerVariable + 'static>(
    variables: impl Into<Box<[Var]>>,
    constraint_tag: ConstraintTag,
    conflict_detection_only: bool,
) -> impl Constraint {
    let variables: Box<[Var]> = variables.into();
    AllDifferentConstructor {
        x: variables,
        constraint_tag,
        conflict_detection_only,
    }
}
