use pumpkin_core::checkers::Scope;
use pumpkin_core::propagation::ConstraintDescription;
use pumpkin_core::propagation::LocalId;
use pumpkin_core::variables::IntegerVariable;

use super::ID_INDEX;
use super::ID_RHS;
use super::ID_X_OFFSET;

/// The description of the constraint `array[index] = rhs`.
#[derive(Clone, Debug)]
pub struct ElementDescription<VX, VI, VE> {
    pub array: Box<[VX]>,
    pub index: VI,
    pub rhs: VE,
}

impl<VX, VI, VE> ConstraintDescription for ElementDescription<VX, VI, VE>
where
    VX: IntegerVariable,
    VI: IntegerVariable,
    VE: IntegerVariable,
{
    fn scope(&self) -> Scope {
        let mut scope = Scope::default();
        for (i, x_i) in self.array.iter().enumerate() {
            x_i.add_to_scope(&mut scope, LocalId::from(i as u32 + ID_X_OFFSET));
        }
        self.index.add_to_scope(&mut scope, ID_INDEX);
        self.rhs.add_to_scope(&mut scope, ID_RHS);
        scope
    }
}
