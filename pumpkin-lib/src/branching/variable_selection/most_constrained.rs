use log::warn;

use crate::basic_types::DomainId;
#[cfg(doc)]
use crate::branching::FirstFail;
use crate::branching::SelectionContext;
use crate::branching::VariableSelector;
use crate::pumpkin_assert_eq_simple;

/// A [`VariableSelector`] which selects the variable with the smallest domain (similar to
/// [`FirstFail`]) which breaks ties according to the number of attached
/// constraints (giving priority to variable with more attached constraints).
#[derive(Debug)]
pub struct MostConstrained<Var> {
    variables: Vec<Var>,
    num_occurrences: Vec<u32>,
}

impl<Var: Copy> MostConstrained<Var> {
    pub fn new(variables: &[Var], num_occurrences: &[u32]) -> Self {
        pumpkin_assert_eq_simple!(
            variables.len(), num_occurrences.len(),
            "The number of variables and the number of elements in num_occurrences for the MostConstrained variable selector should be the same"
        );
        if variables.is_empty() {
            warn!("The MostConstrained variable selector was not provided with any variables");
        }
        MostConstrained {
            variables: variables.to_vec(),
            num_occurrences: num_occurrences.to_vec(),
        }
    }
}

impl VariableSelector<DomainId> for MostConstrained<DomainId> {
    fn select_variable(&mut self, context: &SelectionContext) -> Option<DomainId> {
        self.variables
            .iter()
            .enumerate()
            .filter(|(_, variable)| !context.is_integer_fixed(**variable))
            .min_by(|(x_index, x), (y_index, y)| {
                match context
                    .get_size_of_domain(**x)
                    .cmp(&context.get_size_of_domain(**y))
                {
                    std::cmp::Ordering::Equal => {
                        // Note that we are reversing x and y here since we want to find the
                        // variable with the largest number of occurrences but we are performing a
                        // `min_by`
                        self.num_occurrences[*y_index].cmp(&self.num_occurrences[*x_index])
                    }
                    ordering => ordering,
                }
            })
            .map(|(_, variable)| variable)
            .cloned()
    }
}

#[cfg(test)]
mod tests {
    use crate::basic_types::tests::TestRandom;
    use crate::branching::MostConstrained;
    use crate::branching::SelectionContext;
    use crate::branching::VariableSelector;

    #[test]
    fn test_correctly_selected() {
        let (mut assignments_integer, assignments_propositional, mediator) =
            SelectionContext::create_for_testing(2, 0, Some(vec![(0, 10), (15, 20)]));
        let mut test_rng = TestRandom::default();
        let integer_variables = assignments_integer.get_domains().collect::<Vec<_>>();
        let mut strategy = MostConstrained::new(&integer_variables, &[2, 1]);

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

        let _ = assignments_integer.tighten_upper_bound(integer_variables[0], 2, None);
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
    fn test_correctly_selected_tie() {
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

        let mut strategy = MostConstrained::new(&integer_variables, &[2, 1]);
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

        let mut strategy = MostConstrained::new(&integer_variables, &[1, 2]);
        let selected = strategy.select_variable(&context);
        assert!(selected.is_none());
    }
}
