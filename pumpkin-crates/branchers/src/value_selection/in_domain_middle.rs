use pumpkin_core::asserts::pumpkin_assert_simple;
use pumpkin_core::branching::BrancherEvent;
use pumpkin_core::branching::SelectionContext;
use pumpkin_core::predicate;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::variables::IntegerVariable;

#[cfg(doc)]
use crate::value_selection::InDomainMedian;
use crate::value_selection::ValueSelector;

/// A [`ValueSelector`] which selects the middle value in the domain (or if this value is already
/// assigned then the closest variable to it).
///
/// Note that this strategy is different from [`InDomainMedian`] if there are holes in the
/// domain.
#[derive(Debug, Copy, Clone)]
pub struct InDomainMiddle;

impl<Var: IntegerVariable + Copy> ValueSelector<Var> for InDomainMiddle {
    fn select_value(
        &mut self,
        context: &mut SelectionContext,
        decision_variable: Var,
    ) -> Predicate {
        let bound = context.lower_bound(decision_variable)
            + (context.get_size_of_domain(decision_variable) as f64 / 2.0).floor() as i32;
        pumpkin_assert_simple!(
            bound >= context.lower_bound(decision_variable)
                && bound < context.upper_bound(decision_variable),
            "It should hold that {} <= {bound} < {}",
            context.lower_bound(decision_variable),
            context.upper_bound(decision_variable)
        );
        // It could be that the domain does not contain the value for bound, we thus look at the
        // left and right to find the closest bound which is in the domain
        let mut offset = 0;
        while bound - offset >= context.lower_bound(decision_variable)
            || bound + offset <= context.upper_bound(decision_variable)
        {
            if context.contains(decision_variable, bound - offset) {
                return predicate!(decision_variable == bound - offset);
            } else if context.contains(decision_variable, bound + offset) {
                return predicate!(decision_variable == bound + offset);
            }
            offset += 1;
        }
        unreachable!("There should be at least 1 selectable variable in the domain");
    }

    fn subscribe_to_events(&self) -> Vec<BrancherEvent> {
        vec![]
    }
}

#[cfg(test)]
mod tests {
    use pumpkin_core::branching::SelectionContext;
    use pumpkin_core::predicate;
    use pumpkin_core::state::State;
    use pumpkin_core::testing::TestRandom;

    use crate::value_selection::InDomainMiddle;
    use crate::value_selection::ValueSelector;

    #[test]
    fn test_returns_correct_literal() {
        let mut state = State::default();
        let domain_ids = [(0, 10)]
            .into_iter()
            .map(|(lower_bound, upper_bound)| {
                state.new_interval_variable(lower_bound, upper_bound, None)
            })
            .collect::<Vec<_>>();

        let mut test_rng = TestRandom::default();
        let mut context = SelectionContext::new(&state, &mut test_rng);

        let mut selector = InDomainMiddle;

        let selected_predicate = selector.select_value(&mut context, domain_ids[0]);
        assert_eq!(selected_predicate, predicate!(domain_ids[0] == 5))
    }

    #[test]
    fn test_returns_correct_literal_no_middle() {
        let mut state = State::default();
        let domain_ids = [(1, 10)]
            .into_iter()
            .map(|(lower_bound, upper_bound)| {
                state.new_interval_variable(lower_bound, upper_bound, None)
            })
            .collect::<Vec<_>>();
        let mut selector = InDomainMiddle;

        let _ = state
            .post(predicate!(domain_ids[0] != 5))
            .expect("Expected posting the predicate to not result in an empty domain");

        let mut test_rng = TestRandom::default();
        let mut context = SelectionContext::new(&state, &mut test_rng);
        let selected_predicate = selector.select_value(&mut context, domain_ids[0]);
        assert_eq!(selected_predicate, predicate!(domain_ids[0] == 4))
    }

    #[test]
    fn test_returns_correct_literal_size_two_domain() {
        let mut state = State::default();
        let domain_ids = [(1, 2)]
            .into_iter()
            .map(|(lower_bound, upper_bound)| {
                state.new_interval_variable(lower_bound, upper_bound, None)
            })
            .collect::<Vec<_>>();

        let mut test_rng = TestRandom::default();
        let mut context = SelectionContext::new(&state, &mut test_rng);

        let mut selector = InDomainMiddle;

        let selected_predicate = selector.select_value(&mut context, domain_ids[0]);
        assert_eq!(selected_predicate, predicate!(domain_ids[0] == 1))
    }

    #[test]
    fn test_returns_correct_literal_size_three_domain() {
        let mut state = State::default();
        let domain_ids = [(1, 3)]
            .into_iter()
            .map(|(lower_bound, upper_bound)| {
                state.new_interval_variable(lower_bound, upper_bound, None)
            })
            .collect::<Vec<_>>();

        let mut test_rng = TestRandom::default();
        let mut context = SelectionContext::new(&state, &mut test_rng);

        let mut selector = InDomainMiddle;

        let selected_predicate = selector.select_value(&mut context, domain_ids[0]);
        assert_eq!(selected_predicate, predicate!(domain_ids[0] == 2))
    }

    #[test]
    fn test_returns_correct_literal_negative_lower_bound() {
        let mut state = State::default();
        let domain_ids = [(-5, 5)]
            .into_iter()
            .map(|(lower_bound, upper_bound)| {
                state.new_interval_variable(lower_bound, upper_bound, None)
            })
            .collect::<Vec<_>>();

        let mut test_rng = TestRandom::default();
        let mut context = SelectionContext::new(&state, &mut test_rng);

        let mut selector = InDomainMiddle;

        let selected_predicate = selector.select_value(&mut context, domain_ids[0]);
        assert_eq!(selected_predicate, predicate!(domain_ids[0] == 0))
    }

    #[test]
    fn test_returns_correct_literal_negative_upper_bound() {
        let mut state = State::default();
        let domain_ids = [(-10, -5)]
            .into_iter()
            .map(|(lower_bound, upper_bound)| {
                state.new_interval_variable(lower_bound, upper_bound, None)
            })
            .collect::<Vec<_>>();

        let mut test_rng = TestRandom::default();
        let mut context = SelectionContext::new(&state, &mut test_rng);

        let mut selector = InDomainMiddle;

        let selected_predicate = selector.select_value(&mut context, domain_ids[0]);
        assert_eq!(selected_predicate, predicate!(domain_ids[0] == -8))
    }
}
