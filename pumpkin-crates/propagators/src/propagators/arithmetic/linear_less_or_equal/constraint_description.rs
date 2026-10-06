use pumpkin_core::checkers::Scope;
use pumpkin_core::propagation::ConstraintDescription;
use pumpkin_core::variables::IntegerVariable;

/// The description of the linear inequality `∑ terms_i <= bound`.
#[derive(Clone, Debug)]
pub struct LinearLessOrEqualDescription<Var> {
    pub terms: Box<[Var]>,
    pub bound: i32,
}

impl<Var: IntegerVariable> ConstraintDescription for LinearLessOrEqualDescription<Var> {
    fn scope(&self) -> Scope {
        Scope::from_variables(self.terms.iter())
    }
}
