use crate::branching::SelectionContext;
use crate::branching::ValueSelector;
use crate::engine::predicates::integer_predicate::IntegerPredicate;
use crate::engine::variables::DomainId;
use crate::predicate;

/// A [`ValueSelector`] which excludes the median value from the domain.
#[derive(Debug, Copy, Clone)]
pub struct OutDomainMedian;

impl ValueSelector<DomainId> for OutDomainMedian {
    fn select_value(
        &mut self,
        context: &mut SelectionContext,
        decision_variable: DomainId,
    ) -> IntegerPredicate {
        let values_in_domain = (context.lower_bound(decision_variable)
            ..=context.upper_bound(decision_variable))
            .filter(|bound| context.contains(decision_variable, *bound))
            .collect::<Vec<_>>();
        predicate!(decision_variable != values_in_domain[values_in_domain.len() / 2])
    }
}

#[cfg(test)]
mod tests {
    use crate::basic_types::tests::TestRandom;
    use crate::branching::OutDomainMedian;
    use crate::branching::SelectionContext;
    use crate::branching::ValueSelector;
    use crate::predicate;

    #[test]
    fn test_returns_correct_literal() {
        let assignments = SelectionContext::create_for_testing(vec![(0, 10)]);
        let mut test_rng = TestRandom::default();
        let mut context = SelectionContext::new(&assignments, &mut test_rng);
        let domain_ids = context.get_domains().collect::<Vec<_>>();

        let mut selector = OutDomainMedian;

        let selected_predicate = selector.select_value(&mut context, domain_ids[0]);
        assert_eq!(selected_predicate, predicate!(domain_ids[0] != 5))
    }

    #[test]
    fn test_returns_correct_literal_no_median() {
        let mut assignments = SelectionContext::create_for_testing(vec![(1, 10)]);
        let mut test_rng = TestRandom::default();
        let domain_ids = assignments.get_domains().collect::<Vec<_>>();

        let mut selector = OutDomainMedian;

        let _ = assignments.remove_value_from_domain(domain_ids[0], 9, None);

        let mut context = SelectionContext::new(&assignments, &mut test_rng);

        let selected_predicate = selector.select_value(&mut context, domain_ids[0]);
        assert_eq!(selected_predicate, predicate!(domain_ids[0] != 5))
    }
}
