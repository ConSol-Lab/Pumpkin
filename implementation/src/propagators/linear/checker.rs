use pumpkin_checking::AtomicConstraint;
use pumpkin_checking::CheckerVariable;
use pumpkin_checking::InferenceChecker;

#[derive(Debug, Clone)]
pub struct LinearChecker<Var> {
    pub x: Vec<Var>,
    pub bound: i32,
}

impl<Atomic: AtomicConstraint, Var: CheckerVariable<Atomic>> InferenceChecker<Atomic>
    for LinearChecker<Var>
{
    fn check(
        &self,
        _state: pumpkin_checking::VariableState<Atomic>,
        _premises: &[Atomic],
        _consequent: Option<&Atomic>,
    ) -> bool {
        todo!()
    }
}
