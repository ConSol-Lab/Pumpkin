use std::borrow::Cow;
use std::marker::PhantomData;

use pumpkin_checking::ConflictChecker;
use pumpkin_checking::RetentionChecker;
use pumpkin_checking::checkers::AbsoluteValueChecker;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::propagation::ConflictRule;
use pumpkin_core::variables::IntegerVariable;

use super::AbsoluteValueDescription;

#[derive(Clone, Copy, Debug)]
pub struct AbsoluteValueRule<VA, VB>(PhantomData<(VA, VB)>);

impl<VA, VB> AbsoluteValueRule<VA, VB>
where
    VA: IntegerVariable + 'static,
    VB: IntegerVariable + 'static,
{
    fn checker(
        constraint_description: &AbsoluteValueDescription<VA, VB>,
    ) -> AbsoluteValueChecker<VA, VB> {
        AbsoluteValueChecker {
            signed: constraint_description.signed.clone(),
            absolute: constraint_description.absolute.clone(),
        }
    }
}

impl<VA, VB> ConflictRule for AbsoluteValueRule<VA, VB>
where
    VA: IntegerVariable + 'static,
    VB: IntegerVariable + 'static,
{
    type Description = AbsoluteValueDescription<VA, VB>;

    fn name() -> Cow<'static, str> {
        Cow::Borrowed("absolute_value")
    }

    fn create_conflict_checker(
        constraint_description: &AbsoluteValueDescription<VA, VB>,
    ) -> impl ConflictChecker<Predicate> + 'static {
        Self::checker(constraint_description)
    }

    fn create_retention_checker(
        constraint_description: &AbsoluteValueDescription<VA, VB>,
    ) -> impl RetentionChecker<Predicate> + 'static {
        Self::checker(constraint_description)
    }
}
