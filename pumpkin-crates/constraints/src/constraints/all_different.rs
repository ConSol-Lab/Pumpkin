use pumpkin_core::constraints::Constraint;
use pumpkin_core::proof::ConstraintTag;
use pumpkin_core::variables::IntegerVariable;
use pumpkin_propagators::disjunctive::ArgDisjunctiveTask;

use crate::constraints::disjunctive_strict;

/// Creates the [`Constraint`] that enforces that all the given `variables` are distinct.
pub fn all_different<Var: IntegerVariable + 'static>(
    variables: impl Into<Box<[Var]>>,
    constraint_tag: ConstraintTag,
) -> impl Constraint {
    let variables: Box<[Var]> = variables.into();

    disjunctive_strict(
        variables
            .iter()
            .map(|variable| ArgDisjunctiveTask {
                start_time: variable.clone(),
                processing_time: 1,
            })
            .collect::<Vec<_>>(),
        constraint_tag,
    )
}
