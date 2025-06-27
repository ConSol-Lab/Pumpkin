use crate::branching::brancher::BrancherEvent;
use crate::branching::value_selection::ValueSelector;
use crate::branching::SelectionContext;
use crate::engine::predicates::predicate::Predicate;
use crate::engine::variables::IntegerVariable;
use crate::predicate;

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
    use crate::basic_types::tests::TestRandom;
    use crate::branching::value_selection::InDomainMedian;
    use crate::branching::value_selection::ValueSelector;
    use crate::branching::SelectionContext;
    use crate::predicate;

    #[test]
    fn test_returns_correct_literal() {
        let (assignments, _) = SelectionContext::create_for_testing(vec![(0, 10)]);
        let mut test_rng = TestRandom::default();
        let mut context = SelectionContext::new(&assignments, &mut test_rng);
        let domain_ids = context.get_domains().collect::<Vec<_>>();

        let mut selector = InDomainMedian;

        let selected_predicate = selector.select_value(&mut context, domain_ids[0]);
        assert_eq!(selected_predicate, predicate!(domain_ids[0] == 5))
    }

    #[test]
    fn test_returns_correct_literal_no_median() {
        let (mut assignments, mut notification_engine) =
            SelectionContext::create_for_testing(vec![(1, 10)]);
        let mut test_rng = TestRandom::default();

        let domain_ids = assignments.get_domains().collect::<Vec<_>>();
        let mut selector = InDomainMedian;

        let _ = assignments.post_predicate(
            predicate!(domain_ids[0] != 9),
            None,
            &mut notification_engine,
        );

        let mut context = SelectionContext::new(&assignments, &mut test_rng);

        let selected_predicate = selector.select_value(&mut context, domain_ids[0]);
        assert_eq!(selected_predicate, predicate!(domain_ids[0] == 5))
    }

    #[test]
    fn test_returns_correct_literal_removed_median() {
        let (mut assignments, mut notification_engine) =
            SelectionContext::create_for_testing(vec![(1, 10)]);
        let mut test_rng = TestRandom::default();

        let domain_ids = assignments.get_domains().collect::<Vec<_>>();
        let mut selector = InDomainMedian;

        let _ = assignments.post_predicate(
            predicate!(domain_ids[0] != 5),
            None,
            &mut notification_engine,
        );

        let mut context = SelectionContext::new(&assignments, &mut test_rng);

        let selected_predicate = selector.select_value(&mut context, domain_ids[0]);
        assert_eq!(selected_predicate, predicate!(domain_ids[0] == 6))
    }
}
