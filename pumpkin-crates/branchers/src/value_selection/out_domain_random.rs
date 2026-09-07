use pumpkin_core::branching::BrancherEvent;
use pumpkin_core::branching::SelectionContext;
use pumpkin_core::predicate;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::variables::DomainId;

use crate::value_selection::ValueSelector;

/// A [`ValueSelector`] which excludes a random value from the domain.
#[derive(Debug, Clone, Copy)]
pub struct OutDomainRandom;

impl ValueSelector<DomainId> for OutDomainRandom {
    fn select_value(
        &mut self,
        context: &mut SelectionContext,
        decision_variable: DomainId,
    ) -> Predicate {
        let values_in_domain = (context.lower_bound(decision_variable)
            ..=context.upper_bound(decision_variable))
            .filter(|domain_value| context.contains(decision_variable, *domain_value))
            .collect::<Vec<_>>();
        let random_index = context
            .random()
            .generate_usize_in_range(0..values_in_domain.len());
        predicate!(decision_variable != values_in_domain[random_index])
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
    use pumpkin_core::testing::TestRandom;

    use crate::value_selection::OutDomainRandom;
    use crate::value_selection::ValueSelector;

    #[test]
    fn test_returns_correct_literal() {
        let fixture = SelectionContext::create_for_testing(vec![(0, 10)]);
        let mut test_random = TestRandom {
            usizes: vec![3],
            ..Default::default()
        };
        let mut context = fixture.context(&mut test_random);
        let domain_ids = context.get_domains().collect::<Vec<_>>();

        let mut selector = OutDomainRandom;

        let selected_predicate = selector.select_value(&mut context, domain_ids[0]);

        assert_eq!(selected_predicate, predicate!(domain_ids[0] != 3))
    }
}
