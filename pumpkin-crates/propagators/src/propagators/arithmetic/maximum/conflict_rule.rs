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
        description: &MaximumDescription<ElementVar, Rhs>,
    ) -> MaximumChecker<ElementVar, Rhs> {
        MaximumChecker {
            array: description.array.clone(),
            rhs: description.rhs.clone(),
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
        description: &MaximumDescription<ElementVar, Rhs>,
    ) -> impl InferenceChecker<Predicate> + 'static {
        Self::checker(description)
    }

    fn create_retention_checker(
        description: &MaximumDescription<ElementVar, Rhs>,
    ) -> impl RetentionChecker<Predicate> + 'static {
        Self::checker(description)
    }
}
