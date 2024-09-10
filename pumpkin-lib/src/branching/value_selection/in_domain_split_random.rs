use crate::branching::SelectionContext;
use crate::branching::ValueSelector;
use crate::engine::predicates::predicate::Predicate;
use crate::engine::variables::DomainId;
use crate::predicate;

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
}

#[cfg(test)]
mod tests {

    use crate::basic_types::tests::TestRandom;
    use crate::branching::InDomainSplitRandom;
    use crate::branching::SelectionContext;
    use crate::branching::ValueSelector;
    use crate::predicate;

    #[test]
    fn test_returns_correct_literal() {
        let assignments = SelectionContext::create_for_testing(vec![(0, 10)]);
        let mut test_random = TestRandom {
            usizes: vec![5],
            bools: vec![true],
        };
        let mut context = SelectionContext::new(&assignments, &mut test_random);
        let domain_ids = context.get_domains().collect::<Vec<_>>();

        let mut selector = InDomainSplitRandom;

        let selected_predicate = selector.select_value(&mut context, domain_ids[0]);

        assert_eq!(selected_predicate, predicate!(domain_ids[0] >= 5))
    }
}
