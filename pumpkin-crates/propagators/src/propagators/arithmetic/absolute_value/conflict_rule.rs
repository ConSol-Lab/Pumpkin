use std::borrow::Cow;
use std::marker::PhantomData;

use pumpkin_checking::ConflictChecker;
use pumpkin_checking::checkers::AbsoluteValueChecker;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::propagation::ConflictRule;
use pumpkin_core::variables::IntegerVariable;

use super::AbsoluteValueDescription;

/// The inference rule of the propagator.
#[derive(Clone, Copy, Debug)]
pub struct AbsoluteValueRule<VA, VB>(PhantomData<(VA, VB)>);

impl<VA: IntegerVariable + 'static, VB: IntegerVariable + 'static> ConflictRule
    for AbsoluteValueRule<VA, VB>
{
    type Description = AbsoluteValueDescription<VA, VB>;

    fn name() -> Cow<'static, str> {
        Cow::Borrowed("absolute_value")
    }

    fn create_conflict_checker(
        constraint_description: &AbsoluteValueDescription<VA, VB>,
    ) -> impl ConflictChecker<Predicate> + 'static {
        AbsoluteValueChecker {
            signed: constraint_description.signed.clone(),
            absolute: constraint_description.absolute.clone(),
        }
    }
}
