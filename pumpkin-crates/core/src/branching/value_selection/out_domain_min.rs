use crate::branching::SelectionContext;
use crate::branching::brancher::BrancherEvent;
use crate::branching::value_selection::ValueSelector;
use crate::engine::predicates::predicate::Predicate;
use crate::engine::variables::DomainId;
use crate::predicate;

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
    use crate::basic_types::tests::TestRandom;
    use crate::branching::SelectionContext;
    use crate::branching::value_selection::OutDomainMin;
    use crate::branching::value_selection::ValueSelector;
    use crate::predicate;

    #[test]
    fn test_returns_correct_literal() {
        let (assignments, _) = SelectionContext::create_for_testing(vec![(0, 10)]);
        let mut test_rng = TestRandom::default();
        let mut context = SelectionContext::new(&assignments, &mut test_rng);
        let domain_ids = context.get_domains().collect::<Vec<_>>();

        let mut selector = OutDomainMin;

        let selected_predicate = selector.select_value(&mut context, domain_ids[0]);
        assert_eq!(selected_predicate, predicate!(domain_ids[0] >= 1))
    }
}
