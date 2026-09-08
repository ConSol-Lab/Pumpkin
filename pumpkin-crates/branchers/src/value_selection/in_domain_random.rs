use pumpkin_core::branching::BrancherEvent;
use pumpkin_core::branching::SelectionContext;
use pumpkin_core::predicate;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::variables::DomainId;
use pumpkin_core::variables::Literal;

use crate::value_selection::ValueSelector;

/// A [`ValueSelector`] which assigns to a random value in the domain.
#[derive(Debug, Clone, Copy)]
pub struct InDomainRandom;

impl ValueSelector<DomainId> for InDomainRandom {
    fn select_value(
        &mut self,
        context: &mut SelectionContext,
        decision_variable: DomainId,
    ) -> Predicate {
        let values_in_domain = (context.lower_bound(decision_variable)
            ..=context.upper_bound(decision_variable))
            .filter(|bound| context.contains(decision_variable, *bound))
            .collect::<Vec<_>>();
        let random_index = context
            .random()
            .generate_usize_in_range(0..values_in_domain.len());
        predicate!(decision_variable == values_in_domain[random_index])
    }

    fn is_restart_pointless(&mut self) -> bool {
        false
    }

    fn subscribe_to_events(&self) -> Vec<BrancherEvent> {
        vec![]
    }
}

impl ValueSelector<Literal> for InDomainRandom {
    fn select_value(
        &mut self,
        context: &mut SelectionContext,
        decision_variable: Literal,
    ) -> Predicate {
        if context.random().generate_bool(0.5) {
            decision_variable.get_true_predicate()
        } else {
            (!decision_variable).get_true_predicate()
        }
    }

    fn is_restart_pointless(&mut self) -> bool {
        false
    }

    fn subscribe_to_events(&self) -> Vec<BrancherEvent> {
        vec![]
    }
}

#[cfg(test)]
mod tests {
    use pumpkin_core::branching::SelectionContext;
    use pumpkin_core::predicate;
    use pumpkin_core::state::State;
    use pumpkin_core::testing::TestRandom;

    use crate::value_selection::InDomainRandom;
    use crate::value_selection::ValueSelector;

    #[test]
    fn test_returns_correct_literal() {
        let mut state = State::default();
        let domain_id = state.new_interval_variable(0, 10, None);

        let mut test_random = TestRandom {
            usizes: vec![3],
            ..Default::default()
        };
        let mut context = SelectionContext::new(&state, &mut test_random);

        let mut selector = InDomainRandom;

        let selected_predicate = selector.select_value(&mut context, domain_id);

        assert_eq!(selected_predicate, predicate!(domain_id == 3))
    }
}
