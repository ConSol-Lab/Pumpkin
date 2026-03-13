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
    Atomic::Identifier: std::fmt::Debug,
{
    fn check(
        &self,
        state: pumpkin_checking::VariableState<Atomic>,
        _premises: &[Atomic],
        _consequent: Option<&Atomic>,
    ) -> bool {
        // The indexes that are part of the current chain. This is reset for every variable that we
        // test for membership of a cycle.
        let mut current_chain = FixedBitSet::with_capacity(self.successors.len());

        // Nodes that we have visited in the search for cycles. Any nodes that we have already
        // tested for cycles by following chains do not have to be tested for cycles starting at
        // them.
        let mut visited = FixedBitSet::with_capacity(self.successors.len());

        for (visiting_idx, successor) in self.successors.iter().enumerate() {
            if visited.contains(visiting_idx) {
                continue;
            }

            visited.insert(visiting_idx);

            let Some(next_node) = successor.induced_fixed_value(&state) else {
                continue;
            };

            // circuit is 1-indexed
            let mut next_idx = usize::try_from(next_node).unwrap() - 1;
            current_chain.clear();

            loop {
                if current_chain.contains(next_idx) {
                    // Here we are in a cycle. That is a conflict if the length of the
                    // cycle is less than the number of nodes in the graph.
                    return current_chain.count_ones(..) < self.successors.len();
                }

                current_chain.insert(next_idx);
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

#[cfg(test)]
mod tests {
    use pumpkin_checking::Comparison;
    use pumpkin_checking::TestAtomic;
    use pumpkin_checking::VariableState;
    use pumpkin_checking::test_atom;

    use super::*;

    #[test]
    fn does_not_detect_cycle_going_backwards() {
        let checker = CircuitChecker {
            successors: ["x1", "x2", "x3", "x4"].into(),
        };

        let premises = [TestAtomic {
            name: "x3",
            comparison: Comparison::Equal,
            value: 2,
        }];

        let state =
            VariableState::prepare_for_conflict_check(premises, None).expect("conflicting domain");

        assert!(!checker.check(state, &premises, None));
    }

    #[test]
    fn detects_cycle_among_multiple_chains() {
        let checker = CircuitChecker {
            successors: ["x1", "x2", "x3", "x4"].into(),
        };

        let premises = [
            TestAtomic {
                name: "x1",
                comparison: Comparison::Equal,
                value: 2,
            },
            TestAtomic {
                name: "x3",
                comparison: Comparison::Equal,
                value: 4,
            },
        ];

        let consequent = TestAtomic {
            name: "x4",
            comparison: Comparison::NotEqual,
            value: 3,
        };

        let state = VariableState::prepare_for_conflict_check(premises, Some(consequent))
            .expect("conflicting domain");

        assert!(checker.check(state, &premises, Some(&consequent)));
    }

    #[test]
    fn hamiltonian_path_is_no_conflict() {
        let premises = [
            test_atom![x1 == 2],
            test_atom![x2 == 3],
            test_atom![x3 == 1],
        ];

        let consequent = Some(test_atom![x1 != 2]);

        let state =
            VariableState::prepare_for_conflict_check(premises, consequent).expect("no conflict");

        let checker = CircuitChecker {
            successors: ["x1", "x2", "x3"].into(),
        };

        assert!(!checker.check(state, &premises, consequent.as_ref()));
    }
}
