use crate::branching::brancher::BrancherEvent;
use crate::branching::value_selection::ValueSelector;
use crate::branching::SelectionContext;
use crate::engine::predicates::predicate::Predicate;
use crate::engine::variables::IntegerVariable;
use crate::predicate;

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
    use crate::basic_types::tests::TestRandom;
    use crate::branching::value_selection::InDomainMax;
    use crate::branching::value_selection::ValueSelector;
    use crate::branching::SelectionContext;
    use crate::predicate;

    #[test]
    fn test_returns_correct_literal() {
        let (assignments, _) = SelectionContext::create_for_testing(vec![(0, 10)]);
        let mut test_rng = TestRandom::default();
        let mut context = SelectionContext::new(&assignments, &mut test_rng);
        let domain_ids = context.get_domains().collect::<Vec<_>>();

        let mut selector = InDomainMax;

        let selected_predicate = selector.select_value(&mut context, domain_ids[0]);
        assert_eq!(selected_predicate, predicate!(domain_ids[0] >= 10))
    }
}
