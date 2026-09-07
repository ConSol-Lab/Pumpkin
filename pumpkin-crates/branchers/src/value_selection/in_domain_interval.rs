use pumpkin_core::branching::BrancherEvent;
use pumpkin_core::branching::SelectionContext;
use pumpkin_core::predicate;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::variables::DomainId;

use super::InDomainSplit;
use crate::value_selection::ValueSelector;

/// Reduces the domain (consisting of intervals) to its first interval.
///
/// If the domain consists of several intervals (e.g. a variable with the domain {0, 1, 4, 5, 6, 9,
/// 10} consists of the interval {[0-1], [4-6], [9-10]}), then this [`ValueSelector`] will reduce
/// the domain to the first interval (e.g. to {0, 1} in the previous example).
///
/// Otherwise (i.e. if the domain is one continuous interval) then it will bisect the domain in the
/// same manner as [`InDomainSplit`].
#[derive(Debug, Copy, Clone)]
pub struct InDomainInterval;

impl ValueSelector<DomainId> for InDomainInterval {
    fn select_value(
        &mut self,
        context: &mut SelectionContext,
        decision_variable: DomainId,
    ) -> Predicate {
        // We attempt to find the first hole in the domain (i.e. the value is not in the domain of
        // `decision_variable`) Note that the lower-bound and upper-bound are guaranteed to
        // be in the domain
        if let Some(first_interval) = (context.lower_bound(decision_variable) + 1
            ..context.upper_bound(decision_variable))
            .find(|bound| !context.contains(decision_variable, *bound))
        {
            // We use `first_interval - 1` since `first_interval` is the value of the first value
            // which is not in the domain (and the upper-bound predicate is inclusive)
            predicate!(decision_variable <= first_interval - 1)
        } else {
            // There are no holes in the domain, we bisect the domain and exclude the upper-half of
            // the domain
            InDomainSplit::get_predicate_excluding_upper_half(context, decision_variable)
        }
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

    use super::InDomainInterval;
    use crate::value_selection::ValueSelector;

    #[test]
    fn test_returns_correct_literal() {
        let mut fixture = SelectionContext::create_for_testing(vec![(0, 10)]);
        let mut test_rng = TestRandom::default();
        let domain_ids = fixture.get_domains().collect::<Vec<_>>();
        let mut selector = InDomainInterval;

        for to_remove in [2, 3, 7, 8] {
            let _ = fixture.post_predicate(predicate!(domain_ids[0] != to_remove));
        }

        let mut context = fixture.context(&mut test_rng);

        let selected_predicate = selector.select_value(&mut context, domain_ids[0]);
        assert_eq!(selected_predicate, predicate!(domain_ids[0] <= 1))
    }

    #[test]
    fn test_no_holes_in_domain_bisects_domain() {
        let fixture = SelectionContext::create_for_testing(vec![(0, 10)]);
        let mut test_rng = TestRandom::default();
        let mut context = fixture.context(&mut test_rng);
        let domain_ids = context.get_domains().collect::<Vec<_>>();

        let mut selector = InDomainInterval;

        let selected_predicate = selector.select_value(&mut context, domain_ids[0]);

        assert_eq!(selected_predicate, predicate!(domain_ids[0] <= 5),)
    }

    #[test]
    fn test_domain_of_size_two() {
        let fixture = SelectionContext::create_for_testing(vec![(1, 2)]);
        let mut test_rng = TestRandom::default();
        let mut context = fixture.context(&mut test_rng);
        let domain_ids = context.get_domains().collect::<Vec<_>>();

        let mut selector = InDomainInterval;

        let selected_predicate = selector.select_value(&mut context, domain_ids[0]);

        assert_eq!(selected_predicate, predicate!(domain_ids[0] <= 1))
    }
}
