use std::borrow::Cow;
use std::marker::PhantomData;

use pumpkin_checking::InferenceChecker;
use pumpkin_checking::RetentionChecker;
use pumpkin_checking::checkers::LinearLessOrEqualChecker;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::propagation::ConflictRule;
use pumpkin_core::variables::IntegerVariable;

use super::LinearLessOrEqualDescription;

/// The rule of the linear inequality.
#[derive(Clone, Copy, Debug)]
pub struct LinearLessOrEqualRule<Var>(PhantomData<Var>);

impl<Var: IntegerVariable + 'static> ConflictRule for LinearLessOrEqualRule<Var> {
    type Description = LinearLessOrEqualDescription<Var>;

    fn name() -> Cow<'static, str> {
        Cow::Borrowed("linear_bounds")
    }

    fn create_inference_checker(
        description: &LinearLessOrEqualDescription<Var>,
    ) -> impl InferenceChecker<Predicate> + 'static {
        LinearLessOrEqualChecker::new(description.terms.clone(), description.bound)
    }

    fn create_retention_checker(
        description: &LinearLessOrEqualDescription<Var>,
    ) -> impl RetentionChecker<Predicate> + 'static {
        LinearLessOrEqualChecker::new(description.terms.clone(), description.bound)
    }
}
