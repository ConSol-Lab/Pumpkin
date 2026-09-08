use log::warn;
use pumpkin_core::asserts::pumpkin_assert_eq_simple;
use pumpkin_core::branching::BrancherEvent;
use pumpkin_core::branching::SelectionContext;
use pumpkin_core::variables::DomainId;

use super::VariableSelector;
use crate::tie_breaking::Direction;
use crate::tie_breaking::InOrderTieBreaker;
use crate::tie_breaking::TieBreaker;

/// A [`VariableSelector`] which selects the variable with the smallest value in its domain.
///
/// Uses a [`TieBreaker`] to break ties, the default is the [`InOrderTieBreaker`] but it is
/// possible to construct the variable selector with a custom [`TieBreaker`] by using
/// the method [`Smallest::with_tie_breaker`].
pub struct Smallest<Var, TieBreaking> {
    variables: Vec<Var>,
    tie_breaker: TieBreaking,
}

impl<Var, TieBreaking> std::fmt::Debug for Smallest<Var, TieBreaking> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Smallest").finish()
    }
}

impl<Var: Clone> Smallest<Var, InOrderTieBreaker<Var, i32>> {
    pub fn new(variables: &[Var]) -> Self {
        if variables.is_empty() {
            warn!("The Smallest variable selector was not provided with any variables");
        }
        Smallest {
            variables: variables.to_vec(),
            tie_breaker: InOrderTieBreaker::new(Direction::Minimum),
        }
    }
}

impl<Var: Clone + 'static, TieBreaking: TieBreaker<Var, i32>> Smallest<Var, TieBreaking> {
    pub fn with_tie_breaker(variables: &[Var], tie_breaker: TieBreaking) -> Self {
        pumpkin_assert_eq_simple!(
            tie_breaker.get_direction(),
            Direction::Minimum,
            "The provided tie-breaker to Smallest attempts to find the Maximum value
             instead of the Minimum value, please ensure that you have passed the correct tie-breaker");
        if variables.is_empty() {
            warn!("The Smallest variable selector was not provided with any variables");
            return Smallest {
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

impl<TieBreaking> VariableSelector<DomainId> for Smallest<DomainId, TieBreaking>
where
    TieBreaking: TieBreaker<DomainId, i32>,
{
    fn select_variable(&mut self, context: &mut SelectionContext) -> Option<DomainId> {
        self.variables
            .iter()
            .filter(|variable| !context.is_integer_fixed(**variable))
            .for_each(|variable| {
                self.tie_breaker
                    .consider(*variable, context.lower_bound(*variable));
            });
        self.tie_breaker.select()
    }

    fn subscribe_to_events(&self) -> Vec<BrancherEvent> {
        vec![]
    }
}

#[cfg(test)]
mod tests {
    use pumpkin_core::predicate;
    use pumpkin_core::state::State;
    use pumpkin_core::testing::TestRandom;

    use super::*;

    #[test]
    fn test_correctly_selected() {
        let mut state = State::default();
        let integer_variables = [(11, 15), (10, 20)]
            .into_iter()
            .map(|(lower_bound, upper_bound)| {
                state.new_interval_variable(lower_bound, upper_bound, None)
            })
            .collect::<Vec<_>>();
        let mut test_rng = TestRandom::default();
        let mut strategy = Smallest::new(&integer_variables);
        {
            let mut context = SelectionContext::new(&state, &mut test_rng);
            let selected = strategy.select_variable(&mut context);
            assert!(selected.is_some());
            assert_eq!(selected.unwrap(), integer_variables[1]);
        }

        let _ = state
            .post(predicate!(integer_variables[1] >= 15))
            .expect("Expected posting the predicate to not result in an empty domain");
        let mut context = SelectionContext::new(&state, &mut test_rng);
        let selected = strategy.select_variable(&mut context);
        assert!(selected.is_some());
        assert_eq!(selected.unwrap(), integer_variables[0]);
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

        let mut strategy = Smallest::new(&integer_variables);
        let selected = strategy.select_variable(&mut context);
        assert!(selected.is_none());
    }
}
