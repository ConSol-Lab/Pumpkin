use fixedbitset::FixedBitSet;
use pumpkin_checking::AtomicConstraint;
use pumpkin_checking::CheckerVariable;
use pumpkin_checking::InferenceChecker;

#[derive(Debug, Clone)]
pub struct CircuitChecker<Var> {
    pub successors: Box<[Var]>,
}

impl<Var, Atomic> InferenceChecker<Atomic> for CircuitChecker<Var>
where
    Var: CheckerVariable<Atomic>,
    Atomic: AtomicConstraint,
{
    fn check(
        &self,
        state: pumpkin_checking::VariableState<Atomic>,
        _premises: &[Atomic],
        _consequent: Option<&Atomic>,
    ) -> bool {
        let mut explored = FixedBitSet::with_capacity(self.successors.len()).clone();
        let start = self
            .successors
            .iter()
            .position(|var| var.induced_fixed_value(&state).is_some());

        if start.is_none() {
            return false;
        }

        let start = start.unwrap();

        explored.insert(start);
        let mut next = (self.successors[start].induced_fixed_value(&state).unwrap() - 1) as usize;

        while let Some(next_fixed) = self.successors[next].induced_fixed_value(&state) {
            if explored.contains(next) {
                return true;
            }
            explored.insert(next);
            next = (next_fixed - 1) as usize;
        }

        return false;
    }
}
