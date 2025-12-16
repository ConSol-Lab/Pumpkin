use implementation::circuit::CircuitConstructor;
use pumpkin_core::constraints::Constraint;
use pumpkin_core::proof::ConstraintTag;
use pumpkin_core::variables::IntegerVariable;

/// Creates the [`Constraint`] that enforces that all the given `variables` are distinct.
pub fn circuit<Var: IntegerVariable + 'static>(
    variables: impl Into<Box<[Var]>>,
    constraint_tag: ConstraintTag,
) -> impl Constraint {
    let variables: Box<[Var]> = variables.into();

    CircuitConstructor {
        successors: variables,
        constraint_tag,
    }
}
