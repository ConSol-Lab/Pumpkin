use std::borrow::Cow;
use std::marker::PhantomData;

use pumpkin_checking::InferenceChecker;
use pumpkin_checking::RetentionChecker;
use pumpkin_checking::checkers::IntegerMultiplicationChecker;
use pumpkin_core::checkers::Scope;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::propagation::ConflictRule;
use pumpkin_core::propagation::ConstraintDescription;
use pumpkin_core::propagation::MissingRetentionChecker;
use pumpkin_core::variables::IntegerVariable;

use super::constructor::ID_A;
use super::constructor::ID_B;
use super::constructor::ID_C;

/// The description of the constraint `a * b = c`.
#[derive(Clone, Debug)]
pub struct IntegerMultiplicationDescription<VA, VB, VC> {
    pub a: VA,
    pub b: VB,
    pub c: VC,
}

impl<VA, VB, VC> ConstraintDescription for IntegerMultiplicationDescription<VA, VB, VC>
where
    VA: IntegerVariable,
    VB: IntegerVariable,
    VC: IntegerVariable,
{
    fn scope(&self) -> Scope {
        Scope::from(((ID_A, &self.a), (ID_B, &self.b), (ID_C, &self.c)))
    }
}

/// The rule of the integer multiplication.
#[derive(Clone, Copy, Debug)]
pub struct IntegerMultiplicationRule<VA, VB, VC>(PhantomData<(VA, VB, VC)>);

impl<VA, VB, VC> ConflictRule for IntegerMultiplicationRule<VA, VB, VC>
where
    VA: IntegerVariable + 'static,
    VB: IntegerVariable + 'static,
    VC: IntegerVariable + 'static,
{
    type Description = IntegerMultiplicationDescription<VA, VB, VC>;

    fn name() -> Cow<'static, str> {
        Cow::Borrowed(IntegerMultiplicationChecker::<VA, VB, VC>::RULE_NAME)
    }

    fn create_inference_checker(
        description: &IntegerMultiplicationDescription<VA, VB, VC>,
    ) -> impl InferenceChecker<Predicate> + 'static {
        IntegerMultiplicationChecker {
            a: description.a.clone(),
            b: description.b.clone(),
            c: description.c.clone(),
        }
    }

    fn create_retention_checker(
        _: &IntegerMultiplicationDescription<VA, VB, VC>,
    ) -> impl RetentionChecker<Predicate> + 'static {
        MissingRetentionChecker::todo("the integer multiplication rule")
    }
}
