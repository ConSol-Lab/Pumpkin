use std::borrow::Cow;
use std::marker::PhantomData;

use pumpkin_checking::InferenceChecker;
use pumpkin_checking::RetentionChecker;
use pumpkin_checking::checkers::IntegerMultiplicationChecker;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::propagation::ConflictRule;
use pumpkin_core::variables::IntegerVariable;

use super::IntegerMultiplicationDescription;

/// The rule of the integer multiplication.
#[derive(Clone, Copy, Debug)]
pub struct IntegerMultiplicationRule<VA, VB, VC>(PhantomData<(VA, VB, VC)>);

impl<VA, VB, VC> IntegerMultiplicationRule<VA, VB, VC>
where
    VA: IntegerVariable + 'static,
    VB: IntegerVariable + 'static,
    VC: IntegerVariable + 'static,
{
    fn checker(
        description: &IntegerMultiplicationDescription<VA, VB, VC>,
    ) -> IntegerMultiplicationChecker<VA, VB, VC> {
        IntegerMultiplicationChecker {
            a: description.a.clone(),
            b: description.b.clone(),
            c: description.c.clone(),
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

    fn create_inference_checker(
        description: &IntegerMultiplicationDescription<VA, VB, VC>,
    ) -> impl InferenceChecker<Predicate> + 'static {
        Self::checker(description)
    }

    fn create_retention_checker(
        description: &IntegerMultiplicationDescription<VA, VB, VC>,
    ) -> impl RetentionChecker<Predicate> + 'static {
        Self::checker(description)
    }
}
