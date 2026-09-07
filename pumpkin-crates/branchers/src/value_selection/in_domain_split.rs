use pumpkin_core::asserts::pumpkin_assert_advanced;
use pumpkin_core::branching::BrancherEvent;
use pumpkin_core::branching::SelectionContext;
use pumpkin_core::predicate;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::variables::IntegerVariable;

use crate::value_selection::ValueSelector;

/// A [`ValueSelector`] which splits the domain in half (based on the lower-bound and upper-bound,
/// disregarding holes) and removes the upper-half from the domain.
///
/// Note that this strategy will not necessarily result in an equal split if there are holes in the
/// domain.
#[derive(Debug, Copy, Clone)]
pub struct InDomainSplit;

impl<Var: IntegerVariable + Copy> ValueSelector<Var> for InDomainSplit {
    fn select_value(
        &mut self,
        context: &mut SelectionContext,
        decision_variable: Var,
    ) -> Predicate {
        InDomainSplit::get_predicate_excluding_upper_half(context, decision_variable)
    }

    fn subscribe_to_events(&self) -> Vec<BrancherEvent> {
        vec![]
    }
}

impl InDomainSplit {
    pub fn get_predicate_excluding_upper_half<Var: IntegerVariable + Copy>(
        context: &SelectionContext,
        decision_variable: Var,
    ) -> Predicate {
        // Note that the domain of the variable should always have at least 2 values in it
        // (otherwise it should have been reported as fixed and not selected)
        let bound = context.lower_bound(decision_variable)
            + (context.get_size_of_domain(decision_variable) as f64 / 2.0).floor() as i32;
        pumpkin_assert_advanced!(
            bound >= context.lower_bound(decision_variable)
                && bound < context.upper_bound(decision_variable),
            "It should hold that {} <= {bound} < {}",
            context.lower_bound(decision_variable),
            context.upper_bound(decision_variable)
        );
        predicate!(decision_variable <= bound)
    }
}

#[cfg(test)]
mod tests {
    use pumpkin_core::branching::SelectionContext;
    use pumpkin_core::predicate;
    use pumpkin_core::testing::TestRandom;

    use crate::value_selection::InDomainSplit;
    use crate::value_selection::ValueSelector;

    #[test]
    fn test_returns_correct_literal() {
        let fixture = SelectionContext::create_for_testing(vec![(0, 10)]);
        let mut test_rng = TestRandom::default();
        let mut context = fixture.context(&mut test_rng);
        let domain_ids = context.get_domains().collect::<Vec<_>>();

        let mut selector = InDomainSplit;

        let selected_predicate = selector.select_value(&mut context, domain_ids[0]);

        assert_eq!(selected_predicate, predicate!(domain_ids[0] <= 5))
    }

    #[test]
    fn test_domain_of_size_two() {
        let fixture = SelectionContext::create_for_testing(vec![(1, 2)]);
        let mut test_rng = TestRandom::default();
        let mut context = fixture.context(&mut test_rng);
        let domain_ids = context.get_domains().collect::<Vec<_>>();

        let mut selector = InDomainSplit;

        let selected_predicate = selector.select_value(&mut context, domain_ids[0]);

        assert_eq!(selected_predicate, predicate!(domain_ids[0] <= 1))
    }
}
