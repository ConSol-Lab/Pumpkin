use fixedbitset::FixedBitSet;
use pumpkin_checking::AtomicConstraint;
use pumpkin_checking::CheckerVariable;
use pumpkin_checking::InferenceChecker;

use crate::circuit::domain_value_to_index;

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
        // Try all the successors as possible starting points
        for successor in self.successors.iter() {
            // Skip if successor is not yet fixed
            let Some(next_node) = successor.induced_fixed_value(&state) else {
                continue;
            };

            // Otherwise, we find the index of the successor in the chain.
            let mut next_idx = domain_value_to_index(next_node);

            // We keep track of the visited elements.
            let mut visited = FixedBitSet::with_capacity(self.successors.len());

            loop {
                if visited.contains(next_idx) {
                    // If we have already seen the node, then we check whether it is a subtour or a
                    // full circuit
                    return visited.count_ones(..) < self.successors.len();
                }

                // Otherwise, we mark the successor as visited.
                visited.insert(next_idx);

                let Some(next_node) = self.successors[next_idx].induced_fixed_value(&state) else {
                    // If there is no fixed successor, then we move to the next element.
                    break;
                };

                // Then we move to the next value in the chain.
                next_idx = domain_value_to_index(next_node);
            }
        }

        false
    }
}
