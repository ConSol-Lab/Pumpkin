use pumpkin_core::constraints::Constraint;
use pumpkin_core::proof::ConstraintTag;
use pumpkin_core::variables::IntegerVariable;

use super::binary_not_equals;

/// Creates the [`Constraint`] that enforces that all the given `variables` are distinct.
pub fn all_different<Var: IntegerVariable + 'static>(
    variables: impl Into<Box<[Var]>>,
    constraint_tag: ConstraintTag,
) -> impl Constraint {
    let variables: Box<[Var]> = variables.into();
    let mut constraints = Vec::new();

    for i in 0..variables.len() {
        for j in i + 1..variables.len() {
            constraints.push(binary_not_equals(
                variables[i].clone(),
                variables[j].clone(),
                constraint_tag,
            ));
        }
    }

    constraints
}
