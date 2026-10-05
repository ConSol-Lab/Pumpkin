use std::borrow::Cow;
use std::marker::PhantomData;

use pumpkin_checking::InferenceChecker;
use pumpkin_checking::RetentionChecker;
use pumpkin_checking::checkers::BinaryNotEqualsChecker;
use pumpkin_core::checkers::Scope;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::propagation::ConflictRule;
use pumpkin_core::propagation::ConstraintDescription;
use pumpkin_core::propagation::LocalId;
use pumpkin_core::variables::IntegerVariable;

/// The description of the constraint `a != b`.
#[derive(Clone, Debug)]
pub struct BinaryNotEqualsDescription<AVar, BVar> {
    pub a: AVar,
    pub b: BVar,
}

impl<AVar: IntegerVariable, BVar: IntegerVariable> ConstraintDescription
    for BinaryNotEqualsDescription<AVar, BVar>
{
    fn scope(&self) -> Scope {
        Scope::from(((LocalId::from(0), &self.a), (LocalId::from(1), &self.b)))
    }
}

/// The rule of the binary disequality.
#[derive(Clone, Copy, Debug)]
pub struct BinaryNotEqualsRule<AVar, BVar>(PhantomData<(AVar, BVar)>);

impl<AVar, BVar> BinaryNotEqualsRule<AVar, BVar>
where
    AVar: IntegerVariable + 'static,
    BVar: IntegerVariable + 'static,
{
    fn checker(
        description: &BinaryNotEqualsDescription<AVar, BVar>,
    ) -> BinaryNotEqualsChecker<AVar, BVar> {
        BinaryNotEqualsChecker {
            lhs: description.a.clone(),
            rhs: description.b.clone(),
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
        Cow::Borrowed(BinaryNotEqualsChecker::<AVar, BVar>::RULE_NAME)
    }

    fn create_inference_checker(
        description: &BinaryNotEqualsDescription<AVar, BVar>,
    ) -> impl InferenceChecker<Predicate> + 'static {
        Self::checker(description)
    }

    fn create_retention_checker(
        description: &BinaryNotEqualsDescription<AVar, BVar>,
    ) -> impl RetentionChecker<Predicate> + 'static {
        Self::checker(description)
    }
}
