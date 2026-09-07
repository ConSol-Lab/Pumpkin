use log::warn;
use pumpkin_core::asserts::pumpkin_assert_eq_simple;
use pumpkin_core::asserts::pumpkin_assert_simple;
use pumpkin_core::branching::BrancherEvent;
use pumpkin_core::branching::SelectionContext;
use pumpkin_core::variables::DomainId;

use crate::tie_breaking::Direction;
use crate::tie_breaking::InOrderTieBreaker;
use crate::tie_breaking::TieBreaker;
use crate::variable_selection::VariableSelector;

/// A [`VariableSelector`] which selects the variable with the largest difference between the two
/// smallest values in its domain.
///
/// Currently, due to the implementation of the domains, in the worst-case this selector will go
/// through all variables and all values between the upper-bound and lower-bound.
///
/// Uses a [`TieBreaker`] to break ties, the default is the [`InOrderTieBreaker`] but it is
/// possible to construct the variable selector with a custom [`TieBreaker`] by using
/// the method [`MaxRegret::with_tie_breaker`].
pub struct MaxRegret<Var, TieBreaking> {
    variables: Vec<Var>,
    tie_breaker: TieBreaking,
}

impl<Var, TieBreaking> std::fmt::Debug for MaxRegret<Var, TieBreaking> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("MaxRegret").finish()
    }
}

impl<Var: Clone + 'static> MaxRegret<Var, InOrderTieBreaker<Var, i32>> {
    pub fn new(variables: &[Var]) -> Self {
        if variables.is_empty() {
            warn!("The MaxRegret variable selector was not provided with any variables");
            return MaxRegret {
                variables: vec![],
                tie_breaker: InOrderTieBreaker::new(Direction::Maximum),
            };
        }
        MaxRegret {
            variables: variables.to_vec(),
            tie_breaker: InOrderTieBreaker::new(Direction::Maximum),
        }
    }
}

impl<Var: Clone + 'static, TieBreaking: TieBreaker<Var, i32>> MaxRegret<Var, TieBreaking> {
    pub fn with_tie_breaker(variables: &[Var], tie_breaker: TieBreaking) -> Self {
        pumpkin_assert_eq_simple!(
            tie_breaker.get_direction(),
            Direction::Maximum,
            "The provided tie-breaker to MaxRegret attempts to find the Minimum value
             instead of the Maximum value, please ensure that you have passed the correct tie-breaker");
        if variables.is_empty() {
            warn!("The MaxRegret variable selector was not provided with any variables");
            return MaxRegret {
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

impl<TieBreaking> VariableSelector<DomainId> for MaxRegret<DomainId, TieBreaking>
where
    TieBreaking: TieBreaker<DomainId, i32>,
{
    fn select_variable(&mut self, context: &mut SelectionContext) -> Option<DomainId> {
        self.variables
            .iter()
            .filter(|variable| !context.is_integer_fixed(**variable))
            .for_each(|variable| {
                let smallest_value = context.lower_bound(*variable);
                let second_smallest_value = (smallest_value + 1..=context.upper_bound(*variable))
                    .find(|bound| context.contains(*variable, *bound));
                pumpkin_assert_simple!(
                    second_smallest_value.is_none()
                        || second_smallest_value.unwrap() > smallest_value
                );
                self.tie_breaker.consider(
                    *variable,
                    second_smallest_value.unwrap_or(smallest_value) - smallest_value,
                )
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
    use pumpkin_core::testing::TestRandom;

    use super::*;

    #[test]
    fn test_correctly_selected() {
        let mut fixture = SelectionContext::create_for_testing(vec![(0, 10), (5, 20)]);
        let mut test_rng = TestRandom::default();
        let integer_variables = fixture.get_domains().collect::<Vec<_>>();
        let mut strategy = MaxRegret::new(&integer_variables);

        let _ = fixture.post_predicate(predicate!(integer_variables[1] != 6));

        {
            let mut context = fixture.context(&mut test_rng);

            let selected = strategy.select_variable(&mut context);
            assert!(selected.is_some());
            assert_eq!(selected.unwrap(), integer_variables[1]);
        }

        let _ = fixture.post_predicate(predicate!(integer_variables[0] != 1));
        let _ = fixture.post_predicate(predicate!(integer_variables[0] != 2));

        let mut context = fixture.context(&mut test_rng);

        let selected = strategy.select_variable(&mut context);
        assert!(selected.is_some());
        assert_eq!(selected.unwrap(), integer_variables[0])
    }

    #[test]
    fn fixed_variables_are_not_selected() {
        let fixture = SelectionContext::create_for_testing(vec![(10, 10), (20, 20)]);
        let mut test_rng = TestRandom::default();
        let mut context = fixture.context(&mut test_rng);
        let integer_variables = context.get_domains().collect::<Vec<_>>();

        let mut strategy = MaxRegret::new(&integer_variables);
        let selected = strategy.select_variable(&mut context);
        assert!(selected.is_none());
    }
}
