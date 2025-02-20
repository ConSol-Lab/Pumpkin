use super::VariableSelector;
use crate::branching::BrancherEvent;
use crate::branching::SelectionContext;
use crate::containers::SparseSet;
use crate::containers::StorageKey;
use crate::variables::DomainId;

/// A [`VariableSelector`] which selects a random unfixed variable.
#[derive(Debug)]
pub struct RandomSelector {
    variables: SparseSet<DomainId>,
}

impl RandomSelector {
    pub fn new(variables: impl IntoIterator<Item = DomainId>) -> Self {
        // Note the -1 due to the fact that the indices of the domain ids start at 1
        Self {
            variables: SparseSet::new(variables.into_iter().collect(), |element| {
                element.index() - 1
            }),
        }
    }
}

impl VariableSelector<DomainId> for RandomSelector {
    fn select_variable(&mut self, context: &mut SelectionContext) -> Option<DomainId> {
        if self.variables.is_empty() {
            return None;
        }

        let mut variable = *self.variables.get(
            context
                .random()
                .generate_usize_in_range(0..self.variables.len()),
        );

        while context.is_integer_fixed(variable) {
            self.variables.remove_temporarily(&variable);
            if self.variables.is_empty() {
                return None;
            }

            variable = *self.variables.get(
                context
                    .random()
                    .generate_usize_in_range(0..self.variables.len()),
            );
        }

        Some(variable)
    }

    fn on_unassign_integer(&mut self, variable: DomainId, _value: i32) {
        self.variables.insert(variable);
    }

    fn is_restart_pointless(&mut self) -> bool {
        false
    }

    fn subscribe_to_events(&self) -> Vec<BrancherEvent> {
        vec![BrancherEvent::UnassignInteger]
    }
}

#[cfg(test)]
mod tests {
    use crate::basic_types::tests::TestRandom;
    use crate::branching::variable_selection::RandomSelector;
    use crate::branching::variable_selection::VariableSelector;
    use crate::branching::SelectionContext;

    #[test]
    fn test_selects_randomly() {
        let assignments = SelectionContext::create_for_testing(vec![(0, 10), (5, 20), (1, 3)]);
        let mut test_rng = TestRandom {
            usizes: vec![1],
            ..Default::default()
        };
        let integer_variables = assignments.get_domains().collect::<Vec<_>>();
        let mut strategy = RandomSelector::new(assignments.get_domains());

        let mut context = SelectionContext::new(&assignments, &mut test_rng);

        let selected = strategy.select_variable(&mut context);
        assert!(selected.is_some());
        assert_eq!(selected.unwrap(), integer_variables[1]);
    }

    #[test]
    fn test_selects_randomly_not_unfixed() {
        let assignments = SelectionContext::create_for_testing(vec![(0, 10), (5, 5), (1, 3)]);
        let mut test_rng = TestRandom {
            usizes: vec![1, 0],
            ..Default::default()
        };
        let integer_variables = assignments.get_domains().collect::<Vec<_>>();
        let mut strategy = RandomSelector::new(assignments.get_domains());

        let mut context = SelectionContext::new(&assignments, &mut test_rng);

        let selected = strategy.select_variable(&mut context);
        assert!(selected.is_some());
        assert_eq!(selected.unwrap(), integer_variables[0]);
    }

    #[test]
    fn test_select_nothing_if_all_fixed() {
        let assignments = SelectionContext::create_for_testing(vec![(0, 0), (5, 5), (1, 1)]);
        let mut test_rng = TestRandom {
            usizes: vec![1, 0, 0],
            ..Default::default()
        };
        let mut strategy = RandomSelector::new(assignments.get_domains());

        let mut context = SelectionContext::new(&assignments, &mut test_rng);

        let selected = strategy.select_variable(&mut context);
        assert!(selected.is_none());
    }

    #[test]
    fn test_select_unfixed_variable_after_fixing() {
        let mut assignments = SelectionContext::create_for_testing(vec![(0, 0), (5, 7), (1, 1)]);
        let mut test_rng = TestRandom {
            usizes: vec![2, 0, 0, 0, 0],
            ..Default::default()
        };
        let integer_variables = assignments.get_domains().collect::<Vec<_>>();
        let mut strategy = RandomSelector::new(assignments.get_domains());

        {
            let mut context = SelectionContext::new(&assignments, &mut test_rng);

            let selected = strategy.select_variable(&mut context);
            assert!(selected.is_some());
            assert_eq!(selected.unwrap(), integer_variables[1]);
        }

        assignments.increase_decision_level();
        let _ = assignments.tighten_lower_bound(integer_variables[1], 7, None);

        {
            let mut context = SelectionContext::new(&assignments, &mut test_rng);

            let selected = strategy.select_variable(&mut context);
            assert!(selected.is_none());
        }

        let _ = assignments.synchronise(0, 0, false);
        strategy.on_unassign_integer(integer_variables[1], 7);
        let mut context = SelectionContext::new(&assignments, &mut test_rng);
        let selected = strategy.select_variable(&mut context);
        assert!(selected.is_some());
        assert_eq!(selected.unwrap(), integer_variables[1]);
    }
}
