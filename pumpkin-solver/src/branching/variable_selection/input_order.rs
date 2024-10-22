use log::warn;

use crate::branching::SelectionContext;
use crate::branching::VariableSelector;
use crate::engine::variables::DomainId;
use crate::engine::variables::PropositionalVariable;

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
    fn select_variable(&mut self, context: &SelectionContext) -> Option<DomainId> {
        self.variables
            .iter()
            .find(|variable| !context.is_integer_fixed(**variable))
            .copied()
    }
}

impl VariableSelector<PropositionalVariable> for InputOrder<PropositionalVariable> {
    fn select_variable(&mut self, context: &SelectionContext) -> Option<PropositionalVariable> {
        self.variables
            .iter()
            .find(|variable| !context.is_propositional_variable_fixed(**variable))
            .copied()
    }
}

#[cfg(test)]
mod tests {
    use crate::basic_types::tests::TestRandom;
    use crate::branching::InputOrder;
    use crate::branching::SelectionContext;
    use crate::branching::VariableSelector;

    #[test]
    fn test_correctly_selected() {
        let (mut assignments_integer, assignments_propositional) =
            SelectionContext::create_for_testing(2, 0, Some(vec![(0, 10), (5, 20)]));
        let mut test_rng = TestRandom::default();
        let integer_variables = assignments_integer.get_domains().collect::<Vec<_>>();
        let mut strategy = InputOrder::new(&integer_variables);

        {
            let context = SelectionContext::new(
                &assignments_integer,
                &assignments_propositional,
                &mut test_rng,
            );

            let selected = strategy.select_variable(&context);
            assert!(selected.is_some());
            assert_eq!(selected.unwrap(), integer_variables[0]);
        }

        let _ = assignments_integer.make_assignment(integer_variables[0], 0, None);

        let context = SelectionContext::new(
            &assignments_integer,
            &assignments_propositional,
            &mut test_rng,
        );

        let selected = strategy.select_variable(&context);
        assert!(selected.is_some());
        assert_eq!(selected.unwrap(), integer_variables[1]);
    }

    #[test]
    fn fixed_variables_are_not_selected() {
        let (assignments_integer, assignments_propositional) =
            SelectionContext::create_for_testing(2, 0, Some(vec![(10, 10), (20, 20)]));
        let mut test_rng = TestRandom::default();
        let context = SelectionContext::new(
            &assignments_integer,
            &assignments_propositional,
            &mut test_rng,
        );
        let integer_variables = context.get_domains().collect::<Vec<_>>();

        let mut strategy = InputOrder::new(&integer_variables);
        let selected = strategy.select_variable(&context);
        assert!(selected.is_none());
    }
}
