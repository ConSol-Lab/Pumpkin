use pumpkin_core::asserts::pumpkin_assert_advanced;
use pumpkin_core::branching::BrancherEvent;
use pumpkin_core::branching::SelectionContext;
use pumpkin_core::predicate;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::variables::IntegerVariable;

use crate::value_selection::ValueSelector;

/// A [`ValueSelector`] which splits the domain in half (based on the lower-bound and upper-bound,
/// disregarding holes) and removes the lower-half from the domain.
///
/// Note that this strategy will not necessarily result in an equal split if there are holes in the
/// domain.
#[derive(Debug, Copy, Clone)]
pub struct ReverseInDomainSplit;

impl<Var: IntegerVariable + Copy> ValueSelector<Var> for ReverseInDomainSplit {
    fn select_value(
        &mut self,
        context: &mut SelectionContext,
        decision_variable: Var,
    ) -> Predicate {
        // Note that the domain of the variable should always have at least 2 values in it
        // (otherwise it should have been reported as fixed and not selected)
        let bound = context.lower_bound(decision_variable)
            + (context.get_size_of_domain(decision_variable) as f64 / 2.0).ceil() as i32;
        pumpkin_assert_advanced!(
            bound > context.lower_bound(decision_variable)
                && bound <= context.upper_bound(decision_variable),
            "It should hold that {} < {bound} <= {}",
            context.lower_bound(decision_variable),
            context.upper_bound(decision_variable)
        );
        predicate!(decision_variable >= bound)
    }

    fn subscribe_to_events(&self) -> Vec<BrancherEvent> {
        vec![]
    }
}

#[cfg(test)]
mod tests {
    use pumpkin_core::state::State;
    use pumpkin_core::testing::TestRandom;

    use super::*;

    #[test]
    fn test_returns_correct_literal() {
        let mut state = State::default();
        let domain_id = state.new_interval_variable(0, 10, None);

        let mut test_rng = TestRandom::default();
        let mut context = SelectionContext::new(&state, &mut test_rng);

        let mut selector = ReverseInDomainSplit;

        let selected_predicate = selector.select_value(&mut context, domain_id);

        assert_eq!(selected_predicate, predicate!(domain_id >= 5))
    }

    #[test]
    fn test_domain_of_size_two() {
        let mut state = State::default();
        let domain_id = state.new_interval_variable(1, 2, None);

        let mut test_rng = TestRandom::default();
        let mut context = SelectionContext::new(&state, &mut test_rng);

        let mut selector = ReverseInDomainSplit;

        let selected_predicate = selector.select_value(&mut context, domain_id);

        assert_eq!(selected_predicate, predicate!(domain_id >= 2))
    }
}
