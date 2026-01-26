use pumpkin_checking::AtomicConstraint;
use pumpkin_checking::CheckerVariable;
use pumpkin_checking::InferenceChecker;

use crate::propagators::cumulative::Task;

#[derive(Debug, Clone)]
pub struct CumulativeChecker<Var> {
    pub tasks: Vec<Task<Var>>,
    pub capacity: u32,
}

impl<Atomic: AtomicConstraint, Var: CheckerVariable<Atomic>> InferenceChecker<Atomic>
    for CumulativeChecker<Var>
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
