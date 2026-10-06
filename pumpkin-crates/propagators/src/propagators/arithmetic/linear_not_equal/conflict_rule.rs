use std::borrow::Cow;
use std::marker::PhantomData;

use pumpkin_checking::InferenceChecker;
use pumpkin_checking::RetentionChecker;
use pumpkin_checking::checkers::LinearNotEqualChecker;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::propagation::ConflictRule;
use pumpkin_core::variables::IntegerVariable;

use super::LinearNotEqualDescription;

/// The rule of the linear disequality.
#[derive(Clone, Copy, Debug)]
pub struct LinearNotEqualRule<Var>(PhantomData<Var>);

impl<Var: IntegerVariable + 'static> LinearNotEqualRule<Var> {
    fn checker(
        constraint_description: &LinearNotEqualDescription<Var>,
    ) -> LinearNotEqualChecker<Var> {
        LinearNotEqualChecker {
            terms: constraint_description.terms.as_ref().into(),
            bound: constraint_description.rhs,
        }
    }
}

impl<Var: IntegerVariable + 'static> ConflictRule for LinearNotEqualRule<Var> {
    type Description = LinearNotEqualDescription<Var>;

    fn name() -> Cow<'static, str> {
        Cow::Borrowed("linear_not_equals")
    }

    fn create_inference_checker(
        constraint_description: &LinearNotEqualDescription<Var>,
    ) -> impl InferenceChecker<Predicate> + 'static {
        Self::checker(constraint_description)
    }

    fn create_retention_checker(
        constraint_description: &LinearNotEqualDescription<Var>,
    ) -> impl RetentionChecker<Predicate> + 'static {
        Self::checker(constraint_description)
    }
}
