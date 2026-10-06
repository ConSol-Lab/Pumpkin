use pumpkin_core::checkers::Scope;
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
