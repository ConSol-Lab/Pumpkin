use pumpkin_core::checkers::Scope;
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
