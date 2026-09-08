use pumpkin_core::branching::BrancherEvent;
use pumpkin_core::branching::SelectionContext;
use pumpkin_core::predicate;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::variables::DomainId;

use crate::value_selection::ValueSelector;

/// A [`ValueSelector`] which bisects the domain in the middle (between the lower-bound and
/// lower-bound, disregarding holes), randomly selecting whether to exclude the lower-half or the
/// upper-half.
#[derive(Debug, Clone, Copy)]
pub struct InDomainSplitRandom;

impl ValueSelector<DomainId> for InDomainSplitRandom {
    fn select_value(
        &mut self,
        context: &mut SelectionContext,
        decision_variable: DomainId,
    ) -> Predicate {
        let bound = context.lower_bound(decision_variable)
            + (context.get_size_of_domain(decision_variable) as f64 / 2.0).floor() as i32;

        if context.random().generate_bool(0.5) {
            predicate!(decision_variable >= bound)
        } else {
            predicate!(decision_variable <= bound)
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

    use crate::value_selection::InDomainSplitRandom;
    use crate::value_selection::ValueSelector;

    #[test]
    fn test_returns_correct_literal() {
        let mut state = State::default();
        let domain_id = state.new_interval_variable(0, 10, None);

        let mut test_random = TestRandom {
            usizes: vec![5],
            bools: vec![true],
            ..Default::default()
        };
        let mut context = SelectionContext::new(&state, &mut test_random);

        let mut selector = InDomainSplitRandom;

        let selected_predicate = selector.select_value(&mut context, domain_id);

        assert_eq!(selected_predicate, predicate!(domain_id >= 5))
    }
}
