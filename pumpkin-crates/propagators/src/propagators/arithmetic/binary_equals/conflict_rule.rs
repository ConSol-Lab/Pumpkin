use std::borrow::Cow;
use std::marker::PhantomData;

use pumpkin_checking::ConflictChecker;
use pumpkin_checking::checkers::BinaryEqualsChecker;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::propagation::ConflictRule;
use pumpkin_core::variables::IntegerVariable;

use super::BinaryEqualsDescription;

/// The inference rule of the propagator.
#[derive(Clone, Copy, Debug)]
pub struct BinaryEqualsRule<AVar, BVar>(PhantomData<(AVar, BVar)>);

impl<AVar: IntegerVariable + 'static, BVar: IntegerVariable + 'static> ConflictRule
    for BinaryEqualsRule<AVar, BVar>
{
    type Description = BinaryEqualsDescription<AVar, BVar>;

    fn name() -> Cow<'static, str> {
        Cow::Borrowed("binary_equals")
    }

    fn create_conflict_checker(
        constraint_description: &BinaryEqualsDescription<AVar, BVar>,
    ) -> impl ConflictChecker<Predicate> + 'static {
        BinaryEqualsChecker {
            lhs: constraint_description.a.clone(),
            rhs: constraint_description.b.clone(),
        }
    }
}
