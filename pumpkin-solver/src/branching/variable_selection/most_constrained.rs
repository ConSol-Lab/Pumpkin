use std::cmp::Ordering;

use log::warn;

use crate::branching::brancher::BrancherEvent;
use crate::branching::tie_breaking::Direction;
use crate::branching::tie_breaking::InOrderTieBreaker;
use crate::branching::tie_breaking::TieBreaker;
#[cfg(doc)]
use crate::branching::variable_selection::FirstFail;
use crate::branching::variable_selection::VariableSelector;
use crate::branching::SelectionContext;
use crate::engine::variables::DomainId;
use crate::pumpkin_assert_eq_simple;

/// A [`VariableSelector`] which selects the variable with the smallest domain (similar to
/// [`FirstFail`]).
///
/// It breaks ties according to the number of attached
/// constraints (giving priority to variable with more attached constraints).
pub struct MostConstrained<Var, TieBreaking> {
    variables: Vec<Var>,
    tie_breaker: TieBreaking,
    num_occurrences: Vec<u32>,
}

impl<Var, TieBreaking> std::fmt::Debug for MostConstrained<Var, TieBreaking> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("MostConstrained").finish()
    }
}

#[derive(PartialEq)]
struct MostConstrainedValue {
    domain_size: i32,
    number_of_attached_constraints: u32,
}

impl PartialOrd for MostConstrainedValue {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match self.domain_size.cmp(&other.domain_size) {
            Ordering::Equal => Some(
                // Note that we are comparing `other` to `self` instead of the normal `self` to
                // `other`, this is because the tie-breaking is minimizing while we want to
                // tie-break in the maximizing direction.
                other
                    .number_of_attached_constraints
                    .cmp(&self.number_of_attached_constraints),
            ),
            ordering => Some(ordering),
        }
    }
}

impl<Var: Copy + 'static> MostConstrained<Var, InOrderTieBreaker<Var, MostConstrainedValue>> {
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
            tie_breaker: InOrderTieBreaker::new(Direction::Minimum),
            num_occurrences: num_occurrences.to_vec(),
        }
    }
}

impl<TieBreaking> VariableSelector<DomainId> for MostConstrained<DomainId, TieBreaking>
where
    TieBreaking: TieBreaker<DomainId, MostConstrainedValue>,
{
    fn select_variable(&mut self, context: &mut SelectionContext) -> Option<DomainId> {
        self.variables
            .iter()
            .enumerate()
            .filter(|(_, variable)| !context.is_integer_fixed(**variable))
            .for_each(|(index, variable)| {
                self.tie_breaker.consider(
                    *variable,
                    MostConstrainedValue {
                        domain_size: context.get_size_of_domain(*variable),
                        number_of_attached_constraints: self.num_occurrences[index],
                    },
                );
            });

        self.tie_breaker.select()
    }

    fn subscribe_to_events(&self) -> Vec<BrancherEvent> {
        vec![]
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::basic_types::tests::TestRandom;

    #[test]
    fn test_correctly_selected() {
        let mut assignments = SelectionContext::create_for_testing(vec![(0, 10), (15, 20)]);
        let mut test_rng = TestRandom::default();
        let integer_variables = assignments.get_domains().collect::<Vec<_>>();
        let mut strategy = MostConstrained::new(&integer_variables, &[2, 1]);

        {
            let mut context = SelectionContext::new(&assignments, &mut test_rng);

            let selected = strategy.select_variable(&mut context);
            assert!(selected.is_some());
            assert_eq!(selected.unwrap(), integer_variables[1]);
        }

        let _ = assignments.tighten_upper_bound(integer_variables[0], 2, None);
        let mut context = SelectionContext::new(&assignments, &mut test_rng);
        let selected = strategy.select_variable(&mut context);
        assert!(selected.is_some());
        assert_eq!(selected.unwrap(), integer_variables[0]);
    }

    #[test]
    fn test_correctly_selected_tie() {
        let assignments = SelectionContext::create_for_testing(vec![(0, 10), (10, 20)]);
        let mut test_rng = TestRandom::default();
        let mut context = SelectionContext::new(&assignments, &mut test_rng);
        let integer_variables = context.get_domains().collect::<Vec<_>>();

        let mut strategy = MostConstrained::new(&integer_variables, &[2, 1]);
        let selected = strategy.select_variable(&mut context);
        assert!(selected.is_some());
        assert_eq!(selected.unwrap(), integer_variables[0])
    }

    #[test]
    fn fixed_variables_are_not_selected() {
        let assignments = SelectionContext::create_for_testing(vec![(10, 10), (20, 20)]);
        let mut test_rng = TestRandom::default();
        let mut context = SelectionContext::new(&assignments, &mut test_rng);
        let integer_variables = context.get_domains().collect::<Vec<_>>();

        let mut strategy = MostConstrained::new(&integer_variables, &[1, 2]);
        let selected = strategy.select_variable(&mut context);
        assert!(selected.is_none());
    }
}
