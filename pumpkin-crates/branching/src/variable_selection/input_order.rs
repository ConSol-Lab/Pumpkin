use log::warn;
use pumpkin_core::branching::BrancherEvent;
use pumpkin_core::branching::SelectionContext;
use pumpkin_core::variables::DomainId;
use pumpkin_core::variables::Literal;

use crate::variable_selection::VariableSelector;

/// A [`VariableSelector`] which selects the first variable which is not fixed given the order in
/// the provided list.
#[derive(Debug)]
pub struct InputOrder<Var> {
    variables: Vec<Var>,
}

impl<Var: Copy> InputOrder<Var> {
    pub fn new(variables: &[Var]) -> Self {
        if variables.is_empty() {
            warn!("The InputOrder variable selector was not provided with any variables");
        }
        InputOrder {
            variables: variables.to_vec(),
        }
    }

    pub fn add_domain(&mut self, var: Var) {
        self.variables.push(var)
    }
}

impl VariableSelector<DomainId> for InputOrder<DomainId> {
    fn select_variable(&mut self, context: &mut SelectionContext) -> Option<DomainId> {
        self.variables
            .iter()
            .find(|variable| !context.is_integer_fixed(**variable))
            .copied()
    }

    fn subscribe_to_events(&self) -> Vec<BrancherEvent> {
        vec![]
    }
}

impl VariableSelector<Literal> for InputOrder<Literal> {
    fn select_variable(&mut self, context: &mut SelectionContext) -> Option<Literal> {
        self.variables
            .iter()
            .find(|&variable| !context.is_predicate_assigned(variable.get_true_predicate()))
            .copied()
    }

    fn subscribe_to_events(&self) -> Vec<BrancherEvent> {
        vec![]
    }
}

#[cfg(test)]
mod tests {
    use pumpkin_core::predicate;
    use pumpkin_core::state::State;

    use super::*;
    use crate::testing::TestRandom;

    #[test]
    fn test_correctly_selected() {
        let mut state = State::default();
        let integer_variables = [(0, 10), (5, 20)]
            .into_iter()
            .map(|(lower_bound, upper_bound)| {
                state.new_interval_variable(lower_bound, upper_bound, None)
            })
            .collect::<Vec<_>>();
        let mut test_rng = TestRandom::default();
        let mut strategy = InputOrder::new(&integer_variables);

        {
            let mut context = SelectionContext::new(&state, &mut test_rng);
            let selected = strategy.select_variable(&mut context);
            assert!(selected.is_some());
            assert_eq!(selected.unwrap(), integer_variables[0]);
        }

        let _ = state
            .post(predicate!(integer_variables[0] == 0))
            .expect("Expected posting the predicate to not result in an empty domain");

        let mut context = SelectionContext::new(&state, &mut test_rng);
        let selected = strategy.select_variable(&mut context);
        assert!(selected.is_some());
        assert_eq!(selected.unwrap(), integer_variables[1]);
    }

    #[test]
    fn fixed_variables_are_not_selected() {
        let mut state = State::default();
        let integer_variables = [(10, 10), (20, 20)]
            .into_iter()
            .map(|(lower_bound, upper_bound)| {
                state.new_interval_variable(lower_bound, upper_bound, None)
            })
            .collect::<Vec<_>>();

        let mut test_rng = TestRandom::default();
        let mut context = SelectionContext::new(&state, &mut test_rng);

        let mut strategy = InputOrder::new(&integer_variables);
        let selected = strategy.select_variable(&mut context);
        assert!(selected.is_none());
    }
}
