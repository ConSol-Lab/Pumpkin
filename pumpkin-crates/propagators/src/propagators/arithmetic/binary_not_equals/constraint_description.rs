use pumpkin_core::checkers::Scope;
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
