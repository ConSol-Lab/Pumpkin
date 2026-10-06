use std::borrow::Cow;
use std::marker::PhantomData;

use pumpkin_checking::ConflictChecker;
use pumpkin_checking::RetentionChecker;
use pumpkin_checking::checkers::IntegerMultiplicationChecker;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::propagation::ConflictRule;
use pumpkin_core::variables::IntegerVariable;

use super::IntegerMultiplicationDescription;

#[derive(Clone, Copy, Debug)]
pub struct IntegerMultiplicationRule<VA, VB, VC>(PhantomData<(VA, VB, VC)>);

impl<VA, VB, VC> IntegerMultiplicationRule<VA, VB, VC>
where
    VA: IntegerVariable + 'static,
    VB: IntegerVariable + 'static,
    VC: IntegerVariable + 'static,
{
    fn checker(
        constraint_description: &IntegerMultiplicationDescription<VA, VB, VC>,
    ) -> IntegerMultiplicationChecker<VA, VB, VC> {
        IntegerMultiplicationChecker {
            a: constraint_description.a.clone(),
            b: constraint_description.b.clone(),
            c: constraint_description.c.clone(),
        }
    }
}

impl<VA, VB, VC> ConflictRule for IntegerMultiplicationRule<VA, VB, VC>
where
    VA: IntegerVariable + 'static,
    VB: IntegerVariable + 'static,
    VC: IntegerVariable + 'static,
{
    type Description = IntegerMultiplicationDescription<VA, VB, VC>;

    fn name() -> Cow<'static, str> {
        Cow::Borrowed("integer_multiplication")
    }

    fn create_conflict_checker(
        constraint_description: &IntegerMultiplicationDescription<VA, VB, VC>,
    ) -> impl ConflictChecker<Predicate> + 'static {
        Self::checker(constraint_description)
    }

    fn create_retention_checker(
        constraint_description: &IntegerMultiplicationDescription<VA, VB, VC>,
    ) -> impl RetentionChecker<Predicate> + 'static {
        Self::checker(constraint_description)
    }
}
