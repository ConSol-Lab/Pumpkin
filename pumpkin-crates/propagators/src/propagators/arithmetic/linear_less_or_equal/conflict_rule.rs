use std::borrow::Cow;
use std::marker::PhantomData;

use pumpkin_checking::ConflictChecker;
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

    fn create_conflict_checker(
        constraint_description: &LinearLessOrEqualDescription<Var>,
    ) -> impl ConflictChecker<Predicate> + 'static {
        LinearLessOrEqualChecker::new(
            constraint_description.terms.clone(),
            constraint_description.bound,
        )
    }

    fn create_retention_checker(
        constraint_description: &LinearLessOrEqualDescription<Var>,
    ) -> impl RetentionChecker<Predicate> + 'static {
        LinearLessOrEqualChecker::new(
            constraint_description.terms.clone(),
            constraint_description.bound,
        )
    }
}
