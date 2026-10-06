use std::borrow::Cow;
use std::marker::PhantomData;

use pumpkin_checking::InferenceChecker;
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
        description: &BinaryEqualsDescription<AVar, BVar>,
    ) -> BinaryEqualsChecker<AVar, BVar> {
        BinaryEqualsChecker {
            lhs: description.a.clone(),
            rhs: description.b.clone(),
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

    fn create_inference_checker(
        description: &BinaryEqualsDescription<AVar, BVar>,
    ) -> impl InferenceChecker<Predicate> + 'static {
        Self::checker(description)
    }

    fn create_retention_checker(
        description: &BinaryEqualsDescription<AVar, BVar>,
    ) -> impl RetentionChecker<Predicate> + 'static {
        Self::checker(description)
    }
}
