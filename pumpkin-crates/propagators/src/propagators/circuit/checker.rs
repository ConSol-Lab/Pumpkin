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
        for successor in self.successors.iter() {
            let Some(next_node) = successor.induced_fixed_value(&state) else {
                continue;
            };

            // circuit is 1-indexed
            let mut next_idx = usize::try_from(next_node).unwrap() - 1;

            let mut visited = FixedBitSet::with_capacity(self.successors.len());

            loop {
                if visited.contains(next_idx) && visited.count_ones(..) < self.successors.len() {
                    return true;
                }

                visited.insert(next_idx);

                let Some(next_node) = self.successors[next_idx].induced_fixed_value(&state) else {
                    break;
                };

                next_idx = usize::try_from(next_node).unwrap() - 1;
            }
        }

        false
    }
}
