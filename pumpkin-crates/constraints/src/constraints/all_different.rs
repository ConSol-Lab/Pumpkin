use pumpkin_core::constraints::Constraint;
use pumpkin_core::proof::ConstraintTag;
use pumpkin_core::variables::IntegerVariable;
use pumpkin_propagators::arithmetic::AllDifferentPairPropagatorArgs;
use pumpkin_propagators::arithmetic::BinaryNotEqualsDescription;

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

/// Creates the [`Constraint`] that enforces that all the given `variables` are distinct, decomposed
/// as [`all_different`] is into a disequality per pair, whose inferences carry the rule of the
/// all-different (`all_different`, the rule of the DRCP checker) instead of `binary_not_equals`.
/// Each such inference, `[x = v] -> [y != v]` or the conflict `[x = v] /\ [y = v]`, is one of
/// the all-different on the pair.
pub fn all_different_under_own_rule<Var: IntegerVariable + 'static>(
    variables: impl Into<Box<[Var]>>,
    constraint_tag: ConstraintTag,
) -> impl Constraint {
    let variables: Box<[Var]> = variables.into();
    let mut constraints = Vec::new();

    for i in 0..variables.len() {
        for j in i + 1..variables.len() {
            constraints.push(AllDifferentPairPropagatorArgs {
                constraint_description: BinaryNotEqualsDescription {
                    a: variables[i].clone(),
                    b: variables[j].clone(),
                },
                constraint_tag,
            });
        }
    }

    constraints
}
