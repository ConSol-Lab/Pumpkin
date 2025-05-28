use crate::branching::value_selection::ValueSelector;
use crate::branching::BrancherEvent;
use crate::branching::SelectionContext;
use crate::engine::predicates::predicate::Predicate;
use crate::engine::variables::DomainId;
use crate::predicate;

/// A [`ValueSelector`] which splits the domain in a random manner (between the lower-bound and
/// lower-bound, disregarding holes), randomly selecting whether to exclude the lower-half or the
/// upper-half.
#[derive(Debug, Clone, Copy)]
pub struct RandomSplitter;

impl ValueSelector<DomainId> for RandomSplitter {
    fn select_value(
        &mut self,
        context: &mut SelectionContext,
        decision_variable: DomainId,
    ) -> Predicate {
        // Randomly generate a value within the lower-bound and upper-bound
        let range =
            context.lower_bound(decision_variable)..context.upper_bound(decision_variable) + 1;
        let bound = context.random().generate_i32_in_range(range);

        // We need to handle two special cases:
        //
        // 1. If the bound is equal to the lower-bound then we need to assign it to this bound since
        //    [x >= lb] is currently true
        // 2. If the bound is equal to the upper-bound then we need to assign it to this bound since
        //    [x <= ub] is currentl true
        if bound == context.lower_bound(decision_variable) {
            return predicate!(decision_variable <= bound);
        } else if bound == context.upper_bound(decision_variable) {
            return predicate!(decision_variable >= bound);
        }

        // Then randomly determine how to split the domain
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

    use crate::basic_types::tests::TestRandom;
    use crate::branching::value_selection::RandomSplitter;
    use crate::branching::value_selection::ValueSelector;
    use crate::branching::SelectionContext;
    use crate::predicate;

    #[test]
    fn test_returns_correct_literal() {
        let (assignments, _) = SelectionContext::create_for_testing(vec![(0, 10)]);
        let mut test_random = TestRandom {
            integers: vec![2],
            bools: vec![true],
            ..Default::default()
        };
        let mut context = SelectionContext::new(&assignments, &mut test_random);
        let domain_ids = context.get_domains().collect::<Vec<_>>();

        let mut selector = RandomSplitter;

        let selected_predicate = selector.select_value(&mut context, domain_ids[0]);

        assert_eq!(selected_predicate, predicate!(domain_ids[0] >= 2))
    }
}
