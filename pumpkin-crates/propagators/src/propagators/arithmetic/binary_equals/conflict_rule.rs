use std::borrow::Cow;
use std::marker::PhantomData;

use pumpkin_checking::ConflictChecker;
use pumpkin_checking::RetentionChecker;
use pumpkin_checking::checkers::BinaryEqualsChecker;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::propagation::ConflictRule;
use pumpkin_core::variables::IntegerVariable;

use super::BinaryEqualsDescription;

/// The rule of the binary equality.
#[derive(Clone, Copy, Debug)]
pub struct BinaryEqualsRule<AVar, BVar>(PhantomData<(AVar, BVar)>);

impl<AVar, BVar> BinaryEqualsRule<AVar, BVar>
where
    AVar: IntegerVariable + 'static,
    BVar: IntegerVariable + 'static,
{
    fn checker(
        constraint_description: &BinaryEqualsDescription<AVar, BVar>,
    ) -> BinaryEqualsChecker<AVar, BVar> {
        BinaryEqualsChecker {
            lhs: constraint_description.a.clone(),
            rhs: constraint_description.b.clone(),
        }
    }
}

impl<AVar, BVar> ConflictRule for BinaryEqualsRule<AVar, BVar>
where
    AVar: IntegerVariable + 'static,
    BVar: IntegerVariable + 'static,
{
    type Description = BinaryEqualsDescription<AVar, BVar>;

    fn name() -> Cow<'static, str> {
        Cow::Borrowed("binary_equals")
    }

    fn create_conflict_checker(
        constraint_description: &BinaryEqualsDescription<AVar, BVar>,
    ) -> impl ConflictChecker<Predicate> + 'static {
        Self::checker(constraint_description)
    }

    fn create_retention_checker(
        constraint_description: &BinaryEqualsDescription<AVar, BVar>,
    ) -> impl RetentionChecker<Predicate> + 'static {
        Self::checker(constraint_description)
    }
}
