use log::warn;

use crate::branching::brancher::BrancherEvent;
use crate::branching::tie_breaking::Direction;
use crate::branching::tie_breaking::InOrderTieBreaker;
use crate::branching::tie_breaking::TieBreaker;
use crate::branching::variable_selection::VariableSelector;
use crate::branching::SelectionContext;
use crate::engine::variables::DomainId;
use crate::pumpkin_assert_eq_simple;

/// A [`VariableSelector`] which selects the variable with the largest value in its domain.
///
/// Uses a [`TieBreaker`] to break ties, the default is the [`InOrderTieBreaker`] but it is
/// possible to construct the variable selector with a custom [`TieBreaker`] by using the
/// method [`Largest::with_tie_breaker`].
pub struct Largest<Var, TieBreaking> {
    variables: Vec<Var>,
    tie_breaker: TieBreaking,
}

impl<Var, TieBreaking> std::fmt::Debug for Largest<Var, TieBreaking> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Largest").finish()
    }
}

impl<Var: Clone + 'static> Largest<Var, InOrderTieBreaker<Var, i32>> {
    pub fn new(variables: &[Var]) -> Self {
        if variables.is_empty() {
            warn!("The Largest variable selector was not provided with any variables");
            return Largest {
                variables: vec![],
                tie_breaker: InOrderTieBreaker::new(Direction::Maximum),
            };
        }
        Self {
            variables: variables.to_vec(),
            tie_breaker: InOrderTieBreaker::new(Direction::Maximum),
        }
    }
}

impl<Var: Clone + 'static, TieBreaking: TieBreaker<Var, i32>> Largest<Var, TieBreaking> {
    pub fn with_tie_breaker(variables: &[Var], tie_breaker: TieBreaking) -> Self {
        pumpkin_assert_eq_simple!(
            tie_breaker.get_direction(),
            Direction::Maximum,
            "The provided tie-breaker to Largest attempts to find the Minimum value
             instead of the Maximum value, please ensure that you have passed the correct tie-breaker");
        if variables.is_empty() {
            warn!("The Largest variable selector was not provided with any variables");
            return Largest {
                variables: vec![],
                tie_breaker,
            };
        }

        Self {
            variables: variables.to_vec(),
            tie_breaker,
        }
    }
}

impl<TieBreaking> VariableSelector<DomainId> for Largest<DomainId, TieBreaking>
where
    TieBreaking: TieBreaker<DomainId, i32>,
{
    fn select_variable(&mut self, context: &mut SelectionContext) -> Option<DomainId> {
        self.variables
            .iter()
            .filter(|variable| !context.is_integer_fixed(**variable))
            .for_each(|variable| {
                self.tie_breaker
                    .consider(*variable, context.upper_bound(*variable));
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
        let mut assignments = SelectionContext::create_for_testing(vec![(0, 10), (5, 20)]);
        let mut test_rng = TestRandom::default();
        let integer_variables = assignments.get_domains().collect::<Vec<_>>();
        let mut strategy = Largest::new(&integer_variables);

        {
            let mut context = SelectionContext::new(&assignments, &mut test_rng);

            let selected = strategy.select_variable(&mut context);
            assert!(selected.is_some());
            assert_eq!(selected.unwrap(), integer_variables[1]);
        }

        let _ = assignments.tighten_upper_bound(integer_variables[1], 9, None);

        let mut context = SelectionContext::new(&assignments, &mut test_rng);

        let selected = strategy.select_variable(&mut context);
        assert!(selected.is_some());
        assert_eq!(selected.unwrap(), integer_variables[0]);
    }

    #[test]
    fn fixed_variables_are_not_selected() {
        let assignments = SelectionContext::create_for_testing(vec![(10, 10), (20, 20)]);
        let mut test_rng = TestRandom::default();
        let mut context = SelectionContext::new(&assignments, &mut test_rng);
        let integer_variables = context.get_domains().collect::<Vec<_>>();

        let mut strategy = Largest::new(&integer_variables);
        let selected = strategy.select_variable(&mut context);
        assert!(selected.is_none());
    }
}
