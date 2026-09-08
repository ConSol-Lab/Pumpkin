use pumpkin_core::branching::BrancherEvent;
use pumpkin_core::branching::SelectionContext;
use pumpkin_core::predicate;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::variables::DomainId;

use crate::value_selection::ValueSelector;

/// A [`ValueSelector`] which excludes the smallest value from the domain.
#[derive(Debug, Copy, Clone)]
pub struct OutDomainMin;

impl ValueSelector<DomainId> for OutDomainMin {
    fn select_value(
        &mut self,
        context: &mut SelectionContext,
        decision_variable: DomainId,
    ) -> Predicate {
        predicate!(decision_variable >= context.lower_bound(decision_variable) + 1)
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

    use crate::testing::TestRandom;
    use crate::value_selection::OutDomainMin;
    use crate::value_selection::ValueSelector;

    #[test]
    fn test_returns_correct_literal() {
        let mut state = State::default();
        let domain_id = state.new_interval_variable(0, 10, None);

        let mut test_rng = TestRandom::default();
        let mut context = SelectionContext::new(&state, &mut test_rng);

        let mut selector = OutDomainMin;

        let selected_predicate = selector.select_value(&mut context, domain_id);
        assert_eq!(selected_predicate, predicate!(domain_id >= 1))
    }
}
