use pumpkin_core::checkers::Scope;
use pumpkin_core::propagation::ConstraintDescription;
use pumpkin_core::propagation::LocalId;
use pumpkin_core::variables::IntegerVariable;

/// The description of the constraint `rhs = max(array)`.
#[derive(Clone, Debug)]
pub struct MaximumDescription<ElementVar, Rhs> {
    pub array: Box<[ElementVar]>,
    pub rhs: Rhs,
}

impl<ElementVar: IntegerVariable, Rhs: IntegerVariable> ConstraintDescription
    for MaximumDescription<ElementVar, Rhs>
{
    fn scope(&self) -> Scope {
        let mut scope = Scope::from_variables(self.array.iter());
        self.rhs
            .add_to_scope(&mut scope, LocalId::from(self.array.len() as u32));
        scope
    }
}
