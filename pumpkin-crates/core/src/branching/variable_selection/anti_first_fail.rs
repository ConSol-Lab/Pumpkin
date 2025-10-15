use log::warn;

use crate::branching::brancher::BrancherEvent;
use crate::branching::tie_breaking::Direction;
use crate::branching::tie_breaking::InOrderTieBreaker;
use crate::branching::tie_breaking::TieBreaker;
use crate::branching::variable_selection::VariableSelector;
use crate::branching::SelectionContext;
use crate::engine::variables::DomainId;
use crate::pumpkin_assert_eq_simple;

/// A [`VariableSelector`] which selects the variable with the largest domain (based on the
/// lower-bound and upper-bound, disregarding holes).
///
/// Uses a [`TieBreaker`] to break ties, the default is the [`InOrderTieBreaker`] but it is
/// possible to construct the variable selector with a custom [`TieBreaker`] by
/// using the method [`AntiFirstFail::with_tie_breaker`].
pub struct AntiFirstFail<Var, TieBreaking> {
    variables: Vec<Var>,
    tie_breaker: TieBreaking,
}

impl<Var, TieBreaking> std::fmt::Debug for AntiFirstFail<Var, TieBreaking> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("AntiFirstFail").finish()
    }
}

impl<Var: Clone + 'static> AntiFirstFail<Var, InOrderTieBreaker<Var, i32>> {
    pub fn new(variables: &[Var]) -> Self {
        if variables.is_empty() {
            warn!("The AntiFirstFail variable selector was not provided with any variables");
        }
        Self {
            variables: variables.to_vec(),
            tie_breaker: InOrderTieBreaker::new(Direction::Maximum),
        }
    }
}

impl<Var: Clone + 'static, TieBreaking: TieBreaker<Var, i32>> AntiFirstFail<Var, TieBreaking> {
    pub fn with_tie_breaker(variables: &[Var], tie_breaker: TieBreaking) -> Self {
        pumpkin_assert_eq_simple!(
            tie_breaker.get_direction(),
            Direction::Maximum,
            "The provided tie-breaker to AntiFirstFail attempts to find the Minimum value
             instead of the Maximum value, please ensure that you have passed the correct tie-breaker");
        if variables.is_empty() {
            warn!("The AntiFirstFail variable selector was not provided with any variables");
            return AntiFirstFail {
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

impl<TieBreaking: TieBreaker<DomainId, i32>> VariableSelector<DomainId>
    for AntiFirstFail<DomainId, TieBreaking>
{
    fn select_variable(&mut self, context: &mut SelectionContext) -> Option<DomainId> {
        self.variables
            .iter()
            .filter(|variable| !context.is_integer_fixed(**variable))
            .for_each(|variable| {
                self.tie_breaker
                    .consider(*variable, context.get_size_of_domain(*variable));
            });
        self.tie_breaker.select()
    }

    fn subscribe_to_events(&self) -> Vec<BrancherEvent> {
        vec![]
    }
}

#[cfg(test)]
mod tests {
    use super::AntiFirstFail;
    use crate::basic_types::tests::TestRandom;
    use crate::branching::variable_selection::VariableSelector;
    use crate::branching::SelectionContext;
    use crate::predicate;

    #[test]
    fn test_correctly_selected() {
        let (mut assignments, mut notification_engine) =
            SelectionContext::create_for_testing(vec![(0, 10), (5, 20)]);
        let mut test_rng = TestRandom::default();
        let integer_variables = assignments.get_domains().collect::<Vec<_>>();
        let mut strategy = AntiFirstFail::new(&integer_variables);

        {
            let mut context = SelectionContext::new(&assignments, &mut test_rng);

            let selected = strategy.select_variable(&mut context);
            assert!(selected.is_some());
            assert_eq!(selected.unwrap(), integer_variables[1]);
        }

        let _ = assignments.post(
            predicate!(integer_variables[1] >= 15),
            None,
            &mut notification_engine,
        );

        let mut context = SelectionContext::new(&assignments, &mut test_rng);

        let selected = strategy.select_variable(&mut context);
        assert!(selected.is_some());
        assert_eq!(selected.unwrap(), integer_variables[0]);
    }

    #[test]
    fn fixed_variables_are_not_selected() {
        let (assignments, _) = SelectionContext::create_for_testing(vec![(10, 10), (20, 20)]);
        let mut test_rng = TestRandom::default();
        let mut context = SelectionContext::new(&assignments, &mut test_rng);
        let integer_variables = context.get_domains().collect::<Vec<_>>();

        let mut strategy = AntiFirstFail::new(&integer_variables);
        let selected = strategy.select_variable(&mut context);
        assert!(selected.is_none());
    }
}
