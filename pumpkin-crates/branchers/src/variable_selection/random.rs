use pumpkin_core::branching::BrancherEvent;
use pumpkin_core::branching::SelectionContext;
use pumpkin_core::containers::SparseSet;
use pumpkin_core::variables::DomainId;

use super::VariableSelector;

/// A [`VariableSelector`] which selects a random unfixed variable.
#[derive(Debug)]
pub struct RandomSelector {
    variables: SparseSet<DomainId>,
}

impl RandomSelector {
    pub fn new(variables: impl IntoIterator<Item = DomainId>) -> Self {
        // Note the -1 due to the fact that the indices of the domain ids start at 1
        Self {
            variables: SparseSet::new(variables.into_iter().collect()),
        }
    }

    /// Add a domain to consideration in the variable selection.
    pub fn add_domain(&mut self, domain: DomainId) {
        self.variables.insert(domain);
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
    use pumpkin_core::branching::SelectionContext;
    use pumpkin_core::predicate;
    use pumpkin_core::state::State;
    use pumpkin_core::testing::TestRandom;

    use crate::variable_selection::RandomSelector;
    use crate::variable_selection::VariableSelector;

    #[test]
    fn test_selects_randomly() {
        let mut state = State::default();
        let integer_variables = [(0, 10), (5, 20), (1, 3)]
            .into_iter()
            .map(|(lower_bound, upper_bound)| {
                state.new_interval_variable(lower_bound, upper_bound, None)
            })
            .collect::<Vec<_>>();
        let mut test_rng = TestRandom {
            usizes: vec![1],
            ..Default::default()
        };
        let mut strategy = RandomSelector::new(integer_variables.iter().copied());

        let mut context = SelectionContext::new(&state, &mut test_rng);
        let selected = strategy.select_variable(&mut context);
        assert!(selected.is_some());
        assert_eq!(selected.unwrap(), integer_variables[1]);
    }

    #[test]
    fn test_selects_randomly_not_unfixed() {
        let mut state = State::default();
        let integer_variables = [(0, 10), (5, 5), (1, 3)]
            .into_iter()
            .map(|(lower_bound, upper_bound)| {
                state.new_interval_variable(lower_bound, upper_bound, None)
            })
            .collect::<Vec<_>>();
        let mut test_rng = TestRandom {
            usizes: vec![1, 0],
            ..Default::default()
        };
        let mut strategy = RandomSelector::new(integer_variables.iter().copied());

        let mut context = SelectionContext::new(&state, &mut test_rng);
        let selected = strategy.select_variable(&mut context);
        assert!(selected.is_some());
        assert_eq!(selected.unwrap(), integer_variables[0]);
    }

    #[test]
    fn test_select_nothing_if_all_fixed() {
        let mut state = State::default();
        let integer_variables = [(0, 0), (5, 5), (1, 1)]
            .into_iter()
            .map(|(lower_bound, upper_bound)| {
                state.new_interval_variable(lower_bound, upper_bound, None)
            })
            .collect::<Vec<_>>();
        let mut test_rng = TestRandom {
            usizes: vec![1, 0, 0],
            ..Default::default()
        };
        let mut strategy = RandomSelector::new(integer_variables);

        let mut context = SelectionContext::new(&state, &mut test_rng);
        let selected = strategy.select_variable(&mut context);
        assert!(selected.is_none());
    }

    #[test]
    fn test_select_unfixed_variable_after_fixing() {
        let mut state = State::default();
        let integer_variables = [(0, 0), (5, 7), (1, 1)]
            .into_iter()
            .map(|(lower_bound, upper_bound)| {
                state.new_interval_variable(lower_bound, upper_bound, None)
            })
            .collect::<Vec<_>>();
        let mut test_rng = TestRandom {
            usizes: vec![2, 0, 0, 0, 0],
            ..Default::default()
        };
        let mut strategy = RandomSelector::new(integer_variables.iter().copied());

        {
            let mut context = SelectionContext::new(&state, &mut test_rng);
            let selected = strategy.select_variable(&mut context);
            assert!(selected.is_some());
            assert_eq!(selected.unwrap(), integer_variables[1]);
        }

        state.new_checkpoint();
        let _ = state
            .post(predicate!(integer_variables[1] >= 7))
            .expect("Expected posting the predicate to not result in an empty domain");

        {
            let mut context = SelectionContext::new(&state, &mut test_rng);
            let selected = strategy.select_variable(&mut context);
            assert!(selected.is_none());
        }

        let _ = state.restore_to(0);
        strategy.on_unassign_integer(integer_variables[1], 7);
        let mut context = SelectionContext::new(&state, &mut test_rng);
        let selected = strategy.select_variable(&mut context);
        assert!(selected.is_some());
        assert_eq!(selected.unwrap(), integer_variables[1]);
    }
}
