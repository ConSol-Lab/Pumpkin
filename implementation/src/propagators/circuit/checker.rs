use pumpkin_checking::AtomicConstraint;
use pumpkin_checking::CheckerVariable;
use pumpkin_checking::InferenceChecker;

#[derive(Debug, Clone)]
pub struct CircuitChecker<Var> {
    pub successors: Vec<Var>,
}

impl<Atomic: AtomicConstraint, Var: CheckerVariable<Atomic>> InferenceChecker<Atomic>
    for CircuitChecker<Var>
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
