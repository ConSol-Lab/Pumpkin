use std::borrow::Cow;
use std::marker::PhantomData;

use pumpkin_checking::InferenceChecker;
use pumpkin_checking::RetentionChecker;
use pumpkin_checking::checkers::BinaryEqualsChecker;
use pumpkin_core::checkers::Scope;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::propagation::ConflictRule;
use pumpkin_core::propagation::ConstraintDescription;
use pumpkin_core::variables::IntegerVariable;

/// The description of the constraint `a = b`.
#[derive(Clone, Debug)]
pub struct BinaryEqualsDescription<AVar, BVar> {
    pub a: AVar,
    pub b: BVar,
}

impl<AVar: IntegerVariable, BVar: IntegerVariable> ConstraintDescription
    for BinaryEqualsDescription<AVar, BVar>
{
    fn scope(&self) -> Scope {
        Scope::from(((super::ID_LHS, &self.a), (super::ID_RHS, &self.b)))
    }
}

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
        Cow::Borrowed(BinaryEqualsChecker::<AVar, BVar>::RULE_NAME)
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
