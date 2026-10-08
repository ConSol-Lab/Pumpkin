use std::borrow::Cow;
use std::marker::PhantomData;

use pumpkin_checking::ConflictChecker;
use pumpkin_checking::checkers::MaximumChecker;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::propagation::ConflictRule;
use pumpkin_core::variables::IntegerVariable;

use super::MaximumDescription;

/// The inference rule of the propagator.
#[derive(Clone, Copy, Debug)]
pub struct MaximumRule<ElementVar, Rhs>(PhantomData<(ElementVar, Rhs)>);

impl<ElementVar: IntegerVariable + 'static, Rhs: IntegerVariable + 'static> ConflictRule
    for MaximumRule<ElementVar, Rhs>
{
    type Description = MaximumDescription<ElementVar, Rhs>;

    fn name() -> Cow<'static, str> {
        Cow::Borrowed("maximum")
    }

    fn create_conflict_checker(
        constraint_description: &MaximumDescription<ElementVar, Rhs>,
    ) -> impl ConflictChecker<Predicate> + 'static {
        MaximumChecker {
            array: constraint_description.array.clone(),
            rhs: constraint_description.rhs.clone(),
        }
    }
}
