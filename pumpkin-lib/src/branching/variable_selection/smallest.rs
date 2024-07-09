use log::warn;

use super::VariableSelector;
use crate::branching::Direction;
use crate::branching::InOrderTieBreaker;
use crate::branching::SelectionContext;
use crate::branching::TieBreaker;
use crate::engine::variables::DomainId;
use crate::pumpkin_assert_eq_simple;

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
    fn select_variable(&mut self, context: &SelectionContext) -> Option<DomainId> {
        self.variables
            .iter()
            .filter(|variable| !context.is_integer_fixed(**variable))
            .for_each(|variable| {
                self.tie_breaker
                    .consider(*variable, context.lower_bound(*variable));
            });
        self.tie_breaker.select()
    }
}

#[cfg(test)]
mod tests {
    use crate::basic_types::tests::TestRandom;
    use crate::branching::SelectionContext;
    use crate::branching::Smallest;
    use crate::branching::VariableSelector;

    #[test]
    fn test_correctly_selected() {
        let mut assignments_integer =
            SelectionContext::create_for_testing(vec![(11, 15), (10, 20)]);
        let integer_variables = assignments_integer.get_domains().collect::<Vec<_>>();
        let mut strategy = Smallest::new(&integer_variables);
        let mut test_rng = TestRandom::default();
        {
            let context = SelectionContext::new(&assignments_integer, &mut test_rng);

            let selected = strategy.select_variable(&context);
            assert!(selected.is_some());
            assert_eq!(selected.unwrap(), integer_variables[1]);
        }

        let _ = assignments_integer.tighten_lower_bound(integer_variables[1], 15, None);
        let context = SelectionContext::new(&assignments_integer, &mut test_rng);
        let selected = strategy.select_variable(&context);
        assert!(selected.is_some());
        assert_eq!(selected.unwrap(), integer_variables[0]);
    }

    #[test]
    fn fixed_variables_are_not_selected() {
        let assignments_integer = SelectionContext::create_for_testing(vec![(10, 10), (20, 20)]);
        let mut test_rng = TestRandom::default();
        let context = SelectionContext::new(&assignments_integer, &mut test_rng);
        let integer_variables = context.get_domains().collect::<Vec<_>>();

        let mut strategy = Smallest::new(&integer_variables);
        let selected = strategy.select_variable(&context);
        assert!(selected.is_none());
    }
}
