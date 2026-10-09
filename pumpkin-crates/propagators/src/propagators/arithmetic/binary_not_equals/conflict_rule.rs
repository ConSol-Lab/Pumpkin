use std::borrow::Cow;
use std::marker::PhantomData;

use pumpkin_checking::ConflictChecker;
use pumpkin_checking::RetentionChecker;
use pumpkin_checking::checkers::BinaryNotEqualsChecker;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::propagation::ConflictRule;
use pumpkin_core::variables::IntegerVariable;

use super::BinaryNotEqualsDescription;

#[derive(Clone, Copy, Debug)]
pub struct BinaryNotEqualsRule<AVar, BVar>(PhantomData<(AVar, BVar)>);

impl<AVar, BVar> BinaryNotEqualsRule<AVar, BVar>
where
    AVar: IntegerVariable + 'static,
    BVar: IntegerVariable + 'static,
{
    fn checker(
        constraint_description: &BinaryNotEqualsDescription<AVar, BVar>,
    ) -> BinaryNotEqualsChecker<AVar, BVar> {
        BinaryNotEqualsChecker {
            lhs: constraint_description.a.clone(),
            rhs: constraint_description.b.clone(),
        }
    }
}

impl<AVar, BVar> ConflictRule for BinaryNotEqualsRule<AVar, BVar>
where
    AVar: IntegerVariable + 'static,
    BVar: IntegerVariable + 'static,
{
    type Description = BinaryNotEqualsDescription<AVar, BVar>;

    fn name() -> Cow<'static, str> {
        Cow::Borrowed("binary_not_equals")
    }

    fn create_conflict_checker(
        constraint_description: &BinaryNotEqualsDescription<AVar, BVar>,
    ) -> impl ConflictChecker<Predicate> + 'static {
        Self::checker(constraint_description)
    }

    fn create_retention_checker(
        constraint_description: &BinaryNotEqualsDescription<AVar, BVar>,
    ) -> impl RetentionChecker<Predicate> + 'static {
        Self::checker(constraint_description)
    }
}
