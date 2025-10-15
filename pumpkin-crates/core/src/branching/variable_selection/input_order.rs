use log::warn;

use crate::branching::brancher::BrancherEvent;
use crate::branching::variable_selection::VariableSelector;
use crate::branching::SelectionContext;
use crate::engine::variables::DomainId;
use crate::variables::Literal;

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
    use super::*;
    use crate::basic_types::tests::TestRandom;
    use crate::predicate;

    #[test]
    fn test_correctly_selected() {
        let (mut assignments, mut notification_engine) =
            SelectionContext::create_for_testing(vec![(0, 10), (5, 20)]);
        let mut test_rng = TestRandom::default();
        let integer_variables = assignments.get_domains().collect::<Vec<_>>();
        let mut strategy = InputOrder::new(&integer_variables);

        {
            let mut context = SelectionContext::new(&assignments, &mut test_rng);

            let selected = strategy.select_variable(&mut context);
            assert!(selected.is_some());
            assert_eq!(selected.unwrap(), integer_variables[0]);
        }

        let _ = assignments.post(
            predicate!(integer_variables[0] == 0),
            None,
            &mut notification_engine,
        );

        let mut context = SelectionContext::new(&assignments, &mut test_rng);

        let selected = strategy.select_variable(&mut context);
        assert!(selected.is_some());
        assert_eq!(selected.unwrap(), integer_variables[1]);
    }

    #[test]
    fn fixed_variables_are_not_selected() {
        let (assignments, _) = SelectionContext::create_for_testing(vec![(10, 10), (20, 20)]);
        let mut test_rng = TestRandom::default();
        let mut context = SelectionContext::new(&assignments, &mut test_rng);
        let integer_variables = context.get_domains().collect::<Vec<_>>();

        let mut strategy = InputOrder::new(&integer_variables);
        let selected = strategy.select_variable(&mut context);
        assert!(selected.is_none());
    }
}
