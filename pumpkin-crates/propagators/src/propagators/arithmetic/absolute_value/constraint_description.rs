use pumpkin_core::checkers::Scope;
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
