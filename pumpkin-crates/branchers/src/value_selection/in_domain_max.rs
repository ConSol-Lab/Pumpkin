use pumpkin_core::branching::BrancherEvent;
use pumpkin_core::branching::SelectionContext;
use pumpkin_core::predicate;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::variables::IntegerVariable;

use crate::value_selection::ValueSelector;

/// [`ValueSelector`] which chooses to assign the provided variable to its upper-bound.
#[derive(Debug, Copy, Clone)]
pub struct InDomainMax;

impl<Var: IntegerVariable + Copy> ValueSelector<Var> for InDomainMax {
    fn select_value(
        &mut self,
        context: &mut SelectionContext,
        decision_variable: Var,
    ) -> Predicate {
        predicate!(decision_variable >= context.upper_bound(decision_variable))
    }

    fn subscribe_to_events(&self) -> Vec<BrancherEvent> {
        vec![]
    }
}

#[cfg(test)]
mod tests {
    use pumpkin_core::branching::SelectionContext;
    use pumpkin_core::predicate;
    use pumpkin_core::testing::TestRandom;

    use crate::value_selection::InDomainMax;
    use crate::value_selection::ValueSelector;

    #[test]
    fn test_returns_correct_literal() {
        let fixture = SelectionContext::create_for_testing(vec![(0, 10)]);
        let mut test_rng = TestRandom::default();
        let mut context = fixture.context(&mut test_rng);
        let domain_ids = context.get_domains().collect::<Vec<_>>();

        let mut selector = InDomainMax;

        let selected_predicate = selector.select_value(&mut context, domain_ids[0]);
        assert_eq!(selected_predicate, predicate!(domain_ids[0] >= 10))
    }
}
