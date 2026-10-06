use std::borrow::Cow;
use std::marker::PhantomData;

use pumpkin_checking::InferenceChecker;
use pumpkin_checking::RetentionChecker;
use pumpkin_checking::checkers::MaximumChecker;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::propagation::ConflictRule;
use pumpkin_core::variables::IntegerVariable;

use super::MaximumDescription;

/// The rule of the maximum constraint.
#[derive(Clone, Copy, Debug)]
pub struct MaximumRule<ElementVar, Rhs>(PhantomData<(ElementVar, Rhs)>);

impl<ElementVar, Rhs> MaximumRule<ElementVar, Rhs>
where
    ElementVar: IntegerVariable + 'static,
    Rhs: IntegerVariable + 'static,
{
    fn checker(
        constraint_description: &MaximumDescription<ElementVar, Rhs>,
    ) -> MaximumChecker<ElementVar, Rhs> {
        MaximumChecker {
            array: constraint_description.array.clone(),
            rhs: constraint_description.rhs.clone(),
        }
    }
}

impl<ElementVar, Rhs> ConflictRule for MaximumRule<ElementVar, Rhs>
where
    ElementVar: IntegerVariable + 'static,
    Rhs: IntegerVariable + 'static,
{
    type Description = MaximumDescription<ElementVar, Rhs>;

    fn name() -> Cow<'static, str> {
        Cow::Borrowed("maximum")
    }

    fn create_inference_checker(
        constraint_description: &MaximumDescription<ElementVar, Rhs>,
    ) -> impl InferenceChecker<Predicate> + 'static {
        Self::checker(constraint_description)
    }

    fn create_retention_checker(
        constraint_description: &MaximumDescription<ElementVar, Rhs>,
    ) -> impl RetentionChecker<Predicate> + 'static {
        Self::checker(constraint_description)
    }
}
