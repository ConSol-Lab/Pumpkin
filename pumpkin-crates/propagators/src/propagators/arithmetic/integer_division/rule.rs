use std::borrow::Cow;
use std::marker::PhantomData;

use pumpkin_checking::InferenceChecker;
use pumpkin_checking::RetentionChecker;
use pumpkin_checking::checkers::IntegerDivisionChecker;
use pumpkin_core::checkers::Scope;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::propagation::ConflictRule;
use pumpkin_core::propagation::ConstraintDescription;
use pumpkin_core::variables::IntegerVariable;

use super::ID_DENOMINATOR;
use super::ID_NUMERATOR;
use super::ID_RHS;

/// The description of the constraint `numerator / denominator = rhs`.
#[derive(Clone, Debug)]
pub struct DivisionDescription<VA, VB, VC> {
    pub numerator: VA,
    pub denominator: VB,
    pub rhs: VC,
}

impl<VA, VB, VC> ConstraintDescription for DivisionDescription<VA, VB, VC>
where
    VA: IntegerVariable,
    VB: IntegerVariable,
    VC: IntegerVariable,
{
    fn scope(&self) -> Scope {
        Scope::from((
            (ID_NUMERATOR, &self.numerator),
            (ID_DENOMINATOR, &self.denominator),
            (ID_RHS, &self.rhs),
        ))
    }
}

/// The rule of the integer division.
#[derive(Clone, Copy, Debug)]
pub struct DivisionRule<VA, VB, VC>(PhantomData<(VA, VB, VC)>);

impl<VA, VB, VC> DivisionRule<VA, VB, VC>
where
    VA: IntegerVariable + 'static,
    VB: IntegerVariable + 'static,
    VC: IntegerVariable + 'static,
{
    fn checker(
        description: &DivisionDescription<VA, VB, VC>,
    ) -> IntegerDivisionChecker<VA, VB, VC> {
        IntegerDivisionChecker {
            numerator: description.numerator.clone(),
            denominator: description.denominator.clone(),
            rhs: description.rhs.clone(),
        }
    }
}

impl<VA, VB, VC> ConflictRule for DivisionRule<VA, VB, VC>
where
    VA: IntegerVariable + 'static,
    VB: IntegerVariable + 'static,
    VC: IntegerVariable + 'static,
{
    type Description = DivisionDescription<VA, VB, VC>;

    fn name() -> Cow<'static, str> {
        Cow::Borrowed("division")
    }

    fn create_inference_checker(
        description: &DivisionDescription<VA, VB, VC>,
    ) -> impl InferenceChecker<Predicate> + 'static {
        Self::checker(description)
    }

    fn create_retention_checker(
        description: &DivisionDescription<VA, VB, VC>,
    ) -> impl RetentionChecker<Predicate> + 'static {
        Self::checker(description)
    }
}
