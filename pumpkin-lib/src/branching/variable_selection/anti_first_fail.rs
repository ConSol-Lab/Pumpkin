use log::warn;

use super::variable_selector::find_extremum;
use super::variable_selector::Direction;
use crate::basic_types::DomainId;
use crate::branching::SelectionContext;
use crate::branching::VariableSelector;

/// A [`VariableSelector`] which selects the variable with the largest domain (based on the
/// lower-bound and upper-bound, disregarding holes).
#[derive(Debug)]
pub struct AntiFirstFail<Var> {
    variables: Vec<Var>,
}

impl<Var: Clone> AntiFirstFail<Var> {
    pub fn new(variables: &[Var]) -> Self {
        if variables.is_empty() {
            warn!("The AntiFirstFail variable selector was not provided with any variables");
        }
        AntiFirstFail {
            variables: variables.to_vec(),
        }
    }
}

impl VariableSelector<DomainId> for AntiFirstFail<DomainId> {
    fn select_variable(&mut self, context: &SelectionContext) -> Option<DomainId> {
        find_extremum(
            &self.variables,
            |variable| context.get_size_of_domain(variable),
            context,
            Direction::Maximum,
        )
    }
}

#[cfg(test)]
mod tests {
    use super::AntiFirstFail;
    use crate::basic_types::tests::TestRandom;
    use crate::branching::SelectionContext;
    use crate::branching::VariableSelector;

    #[test]
    fn test_correctly_selected() {
        let (mut assignments_integer, assignments_propositional, mediator) =
            SelectionContext::create_for_testing(2, 0, Some(vec![(0, 10), (5, 20)]));
        let mut test_rng = TestRandom::default();
        let integer_variables = assignments_integer.get_domains().collect::<Vec<_>>();
        let mut strategy = AntiFirstFail::new(&integer_variables);

        {
            let context = SelectionContext::new(
                &assignments_integer,
                &assignments_propositional,
                &mediator,
                &mut test_rng,
            );

            let selected = strategy.select_variable(&context);
            assert!(selected.is_some());
            assert_eq!(selected.unwrap(), integer_variables[1]);
        }

        let _ = assignments_integer.tighten_lower_bound(integer_variables[1], 15, None);

        let context = SelectionContext::new(
            &assignments_integer,
            &assignments_propositional,
            &mediator,
            &mut test_rng,
        );

        let selected = strategy.select_variable(&context);
        assert!(selected.is_some());
        assert_eq!(selected.unwrap(), integer_variables[0]);
    }

    #[test]
    fn fixed_variables_are_not_selected() {
        let (assignments_integer, assignments_propositional, mediator) =
            SelectionContext::create_for_testing(2, 0, Some(vec![(10, 10), (20, 20)]));
        let mut test_rng = TestRandom::default();
        let context = SelectionContext::new(
            &assignments_integer,
            &assignments_propositional,
            &mediator,
            &mut test_rng,
        );
        let integer_variables = context.get_domains().collect::<Vec<_>>();

        let mut strategy = AntiFirstFail::new(&integer_variables);
        let selected = strategy.select_variable(&context);
        assert!(selected.is_none());
    }
}
