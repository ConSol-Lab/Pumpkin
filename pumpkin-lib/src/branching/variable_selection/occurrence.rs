use log::warn;

use crate::basic_types::DomainId;
use crate::branching::SelectionContext;
use crate::branching::VariableSelector;
use crate::pumpkin_assert_eq_simple;

/// A [`VariableSelector`] which selects the variable with the largest number of attached
/// constraints (where [`num_occurrences`][Occurrence::num_occurrences] stores the number of
/// attached constraints per variable).
#[derive(Debug)]
pub struct Occurrence<Var> {
    variables: Vec<Var>,
    num_occurrences: Vec<u32>,
}

impl<Var: Copy> Occurrence<Var> {
    pub fn new(variables: &[Var], num_occurrences: &[u32]) -> Self {
        pumpkin_assert_eq_simple!(
            variables.len(), num_occurrences.len(),
            "The number of variables and the number of elements in num_occurrences for the Occurence variable selector should be the same"
        );
        if variables.is_empty() {
            warn!("The Occurence variable selector was not provided with any variables");
        }
        Occurrence {
            variables: variables.to_vec(),
            num_occurrences: num_occurrences.to_vec(),
        }
    }
}

impl VariableSelector<DomainId> for Occurrence<DomainId> {
    fn select_variable(&mut self, context: &SelectionContext) -> Option<DomainId> {
        self.variables
            .iter()
            .enumerate()
            .filter(|(_, variable)| !context.is_integer_fixed(**variable))
            .max_by(|(x_index, _), (y_index, _)| {
                self.num_occurrences[*x_index].cmp(&self.num_occurrences[*y_index])
            })
            .map(|(_, variable)| variable)
            .cloned()
    }
}

#[cfg(test)]
mod tests {
    use crate::basic_types::tests::TestRandom;
    use crate::branching::Occurrence;
    use crate::branching::SelectionContext;
    use crate::branching::VariableSelector;

    #[test]
    fn test_correctly_selected() {
        let (assignments_integer, assignments_propositional, mediator) =
            SelectionContext::create_for_testing(2, 0, Some(vec![(0, 10), (10, 20)]));
        let mut test_rng = TestRandom::default();
        let context = SelectionContext::new(
            &assignments_integer,
            &assignments_propositional,
            &mediator,
            &mut test_rng,
        );
        let integer_variables = context.get_domains().collect::<Vec<_>>();

        let mut strategy = Occurrence::new(&integer_variables, &[2, 1]);
        let selected = strategy.select_variable(&context);
        assert!(selected.is_some());
        assert_eq!(selected.unwrap(), integer_variables[0])
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

        let mut strategy = Occurrence::new(&integer_variables, &[1, 2]);
        let selected = strategy.select_variable(&context);
        assert!(selected.is_none());
    }
}
