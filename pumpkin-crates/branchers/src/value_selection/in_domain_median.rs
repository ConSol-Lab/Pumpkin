use pumpkin_core::branching::BrancherEvent;
use pumpkin_core::branching::SelectionContext;
use pumpkin_core::predicate;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::variables::IntegerVariable;

use crate::value_selection::ValueSelector;

/// A [`ValueSelector`] which selects the median value in the domain (or if this value is already
/// assigned then the closest variable to it in terms of index).
#[derive(Debug, Copy, Clone)]
pub struct InDomainMedian;

impl<Var: IntegerVariable + Copy> ValueSelector<Var> for InDomainMedian {
    fn select_value(
        &mut self,
        context: &mut SelectionContext,
        decision_variable: Var,
    ) -> Predicate {
        let values_in_domain = (context.lower_bound(decision_variable)
            ..=context.upper_bound(decision_variable))
            .filter(|bound| context.contains(decision_variable, *bound))
            .collect::<Vec<_>>();
        predicate!(decision_variable == values_in_domain[values_in_domain.len() / 2])
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

    use crate::value_selection::InDomainMedian;
    use crate::value_selection::ValueSelector;

    #[test]
    fn test_returns_correct_literal() {
        let mut test_rng = TestRandom::default();
        let mut context = SelectionContext::create_for_testing(vec![(0, 10)], &mut test_rng);
        let domain_ids = context.get_domains().collect::<Vec<_>>();

        let mut selector = InDomainMedian;

        let selected_predicate = selector.select_value(&mut context, domain_ids[0]);
        assert_eq!(selected_predicate, predicate!(domain_ids[0] == 5))
    }

    #[test]
    fn test_returns_correct_literal_no_median() {
        let mut test_rng = TestRandom::default();
        let mut context = SelectionContext::create_for_testing(vec![(1, 10)], &mut test_rng);

        let domain_ids = context.get_domains().collect::<Vec<_>>();
        let mut selector = InDomainMedian;

        let _ = context.post_predicate(predicate!(domain_ids[0] != 9));

        let selected_predicate = selector.select_value(&mut context, domain_ids[0]);
        assert_eq!(selected_predicate, predicate!(domain_ids[0] == 5))
    }

    #[test]
    fn test_returns_correct_literal_removed_median() {
        let mut test_rng = TestRandom::default();
        let mut context = SelectionContext::create_for_testing(vec![(1, 10)], &mut test_rng);

        let domain_ids = context.get_domains().collect::<Vec<_>>();
        let mut selector = InDomainMedian;

        let _ = context.post_predicate(predicate!(domain_ids[0] != 5));

        let selected_predicate = selector.select_value(&mut context, domain_ids[0]);
        assert_eq!(selected_predicate, predicate!(domain_ids[0] == 6))
    }
}
