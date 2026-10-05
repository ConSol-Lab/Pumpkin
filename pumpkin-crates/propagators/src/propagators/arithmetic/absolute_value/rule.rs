use std::borrow::Cow;
use std::marker::PhantomData;

use pumpkin_checking::InferenceChecker;
use pumpkin_checking::RetentionChecker;
use pumpkin_checking::checkers::AbsoluteValueChecker;
use pumpkin_core::checkers::Scope;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::propagation::ConflictRule;
use pumpkin_core::propagation::ConstraintDescription;
use pumpkin_core::propagation::LocalId;
use pumpkin_core::variables::IntegerVariable;

/// The description of the constraint `absolute = |signed|`.
#[derive(Clone, Debug)]
pub struct AbsoluteValueDescription<VA, VB> {
    pub signed: VA,
    pub absolute: VB,
}

impl<VA: IntegerVariable, VB: IntegerVariable> ConstraintDescription
    for AbsoluteValueDescription<VA, VB>
{
    fn scope(&self) -> Scope {
        Scope::from((
            (LocalId::from(0), &self.signed),
            (LocalId::from(1), &self.absolute),
        ))
    }
}

/// The rule of the absolute value constraint.
#[derive(Clone, Copy, Debug)]
pub struct AbsoluteValueRule<VA, VB>(PhantomData<(VA, VB)>);

impl<VA, VB> AbsoluteValueRule<VA, VB>
where
    VA: IntegerVariable + 'static,
    VB: IntegerVariable + 'static,
{
    fn checker(description: &AbsoluteValueDescription<VA, VB>) -> AbsoluteValueChecker<VA, VB> {
        AbsoluteValueChecker {
            signed: description.signed.clone(),
            absolute: description.absolute.clone(),
        }
    }
}

impl<VA, VB> ConflictRule for AbsoluteValueRule<VA, VB>
where
    VA: IntegerVariable + 'static,
    VB: IntegerVariable + 'static,
{
    type Description = AbsoluteValueDescription<VA, VB>;

    fn name() -> Cow<'static, str> {
        Cow::Borrowed(AbsoluteValueChecker::<VA, VB>::RULE_NAME)
    }

    fn create_inference_checker(
        description: &AbsoluteValueDescription<VA, VB>,
    ) -> impl InferenceChecker<Predicate> + 'static {
        Self::checker(description)
    }

    fn create_retention_checker(
        description: &AbsoluteValueDescription<VA, VB>,
    ) -> impl RetentionChecker<Predicate> + 'static {
        Self::checker(description)
    }
}
