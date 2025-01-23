use log::warn;

use crate::branching::brancher::BrancherEvents;
use crate::branching::tie_breaking::Direction;
use crate::branching::tie_breaking::InOrderTieBreaker;
use crate::branching::tie_breaking::TieBreaker;
use crate::branching::variable_selection::VariableSelector;
use crate::branching::SelectionContext;
use crate::engine::variables::DomainId;
use crate::pumpkin_assert_eq_simple;

/// A [`VariableSelector`] which selects the variable with the largest number of attached
/// constraints (where the provided `num_occurrences` stores the number of
/// attached constraints per variable).
pub struct Occurrence<Var, TieBreaking> {
    variables: Vec<Var>,
    tie_breaker: TieBreaking,
    num_occurrences: Vec<u32>,
}

impl<Var, TieBreaking> std::fmt::Debug for Occurrence<Var, TieBreaking> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Occurrence").finish()
    }
}

impl<Var: Copy> Occurrence<Var, InOrderTieBreaker<Var, u32>> {
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
            tie_breaker: InOrderTieBreaker::new(Direction::Maximum),
            num_occurrences: num_occurrences.to_vec(),
        }
    }
}

impl<TieBreaking> VariableSelector<DomainId> for Occurrence<DomainId, TieBreaking>
where
    TieBreaking: TieBreaker<DomainId, u32>,
{
    fn select_variable(&mut self, context: &mut SelectionContext) -> Option<DomainId> {
        self.variables
            .iter()
            .enumerate()
            .filter(|(_, variable)| !context.is_integer_fixed(**variable))
            .for_each(|(index, variable)| {
                self.tie_breaker
                    .consider(*variable, self.num_occurrences[index])
            });
        self.tie_breaker.select()
    }

    fn get_relevant_brancher_events(&self) -> Vec<BrancherEvents> {
        vec![]
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::basic_types::tests::TestRandom;

    #[test]
    fn test_correctly_selected() {
        let assignments = SelectionContext::create_for_testing(vec![(0, 10), (10, 20)]);
        let mut test_rng = TestRandom::default();
        let mut context = SelectionContext::new(&assignments, &mut test_rng);
        let integer_variables = context.get_domains().collect::<Vec<_>>();

        let mut strategy = Occurrence::new(&integer_variables, &[2, 1]);
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

        let mut strategy = Occurrence::new(&integer_variables, &[1, 2]);
        let selected = strategy.select_variable(&mut context);
        assert!(selected.is_none());
    }
}
