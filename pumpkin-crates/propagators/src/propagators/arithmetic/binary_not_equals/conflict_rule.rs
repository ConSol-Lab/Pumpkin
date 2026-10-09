use std::borrow::Cow;
use std::marker::PhantomData;

use pumpkin_checking::ConflictChecker;
use pumpkin_checking::checkers::BinaryNotEqualsChecker;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::propagation::ConflictRule;
use pumpkin_core::variables::IntegerVariable;

use super::BinaryNotEqualsDescription;

/// The inference rule of the propagator.
#[derive(Clone, Copy, Debug)]
pub struct BinaryNotEqualsRule<AVar, BVar>(PhantomData<(AVar, BVar)>);

impl<AVar: IntegerVariable + 'static, BVar: IntegerVariable + 'static> ConflictRule
    for BinaryNotEqualsRule<AVar, BVar>
{
    type Description = BinaryNotEqualsDescription<AVar, BVar>;

    fn name() -> Cow<'static, str> {
        Cow::Borrowed("binary_not_equals")
    }

    fn create_conflict_checker(
        constraint_description: &BinaryNotEqualsDescription<AVar, BVar>,
    ) -> impl ConflictChecker<Predicate> + 'static {
        BinaryNotEqualsChecker {
            lhs: constraint_description.a.clone(),
            rhs: constraint_description.b.clone(),
        }
    }
}
