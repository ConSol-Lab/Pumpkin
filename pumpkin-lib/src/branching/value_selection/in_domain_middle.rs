#[cfg(doc)]
use crate::branching::InDomainMedian;
use crate::branching::SelectionContext;
use crate::branching::ValueSelector;
use crate::engine::predicates::predicate::Predicate;
use crate::engine::variables::IntegerVariable;
use crate::predicate;
use crate::pumpkin_assert_simple;

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
}

#[cfg(test)]
mod tests {
    use crate::basic_types::tests::TestRandom;
    use crate::branching::InDomainMiddle;
    use crate::branching::SelectionContext;
    use crate::branching::ValueSelector;
    use crate::predicate;

    #[test]
    fn test_returns_correct_literal() {
        let (assignments_integer, assignments_propositional) =
            SelectionContext::create_for_testing(1, 0, Some(vec![(0, 10)]));
        let mut test_rng = TestRandom::default();
        let mut context = SelectionContext::new(
            &assignments_integer,
            &assignments_propositional,
            &mut test_rng,
        );
        let domain_ids = context.get_domains().collect::<Vec<_>>();

        let mut selector = InDomainMiddle;

        let selected_predicate = selector.select_value(&mut context, domain_ids[0]);
        assert_eq!(selected_predicate, predicate!(domain_ids[0] == 5))
    }

    #[test]
    fn test_returns_correct_literal_no_middle() {
        let (mut assignments_integer, assignments_propositional) =
            SelectionContext::create_for_testing(1, 0, Some(vec![(1, 10)]));
        let mut test_rng = TestRandom::default();
        let domain_ids = assignments_integer.get_domains().collect::<Vec<_>>();
        let mut selector = InDomainMiddle;

        let _ = assignments_integer.remove_value_from_domain(domain_ids[0], 5, None);

        let mut context = SelectionContext::new(
            &assignments_integer,
            &assignments_propositional,
            &mut test_rng,
        );

        let selected_predicate = selector.select_value(&mut context, domain_ids[0]);
        assert_eq!(selected_predicate, predicate!(domain_ids[0] == 4))
    }

    #[test]
    fn test_returns_correct_literal_size_two_domain() {
        let (assignments_integer, assignments_propositional) =
            SelectionContext::create_for_testing(1, 0, Some(vec![(1, 2)]));
        let mut test_rng = TestRandom::default();
        let mut context = SelectionContext::new(
            &assignments_integer,
            &assignments_propositional,
            &mut test_rng,
        );
        let domain_ids = context.get_domains().collect::<Vec<_>>();

        let mut selector = InDomainMiddle;

        let selected_predicate = selector.select_value(&mut context, domain_ids[0]);
        assert_eq!(selected_predicate, predicate!(domain_ids[0] == 1))
    }

    #[test]
    fn test_returns_correct_literal_size_three_domain() {
        let (assignments_integer, assignments_propositional) =
            SelectionContext::create_for_testing(1, 0, Some(vec![(1, 3)]));
        let mut test_rng = TestRandom::default();
        let mut context = SelectionContext::new(
            &assignments_integer,
            &assignments_propositional,
            &mut test_rng,
        );
        let domain_ids = context.get_domains().collect::<Vec<_>>();

        let mut selector = InDomainMiddle;

        let selected_predicate = selector.select_value(&mut context, domain_ids[0]);
        assert_eq!(selected_predicate, predicate!(domain_ids[0] == 2))
    }

    #[test]
    fn test_returns_correct_literal_negative_lower_bound() {
        let (assignments_integer, assignments_propositional) =
            SelectionContext::create_for_testing(1, 0, Some(vec![(-5, 5)]));
        let mut test_rng = TestRandom::default();
        let mut context = SelectionContext::new(
            &assignments_integer,
            &assignments_propositional,
            &mut test_rng,
        );
        let domain_ids = context.get_domains().collect::<Vec<_>>();

        let mut selector = InDomainMiddle;

        let selected_predicate = selector.select_value(&mut context, domain_ids[0]);
        assert_eq!(selected_predicate, predicate!(domain_ids[0] == 0))
    }

    #[test]
    fn test_returns_correct_literal_negative_upper_bound() {
        let (assignments_integer, assignments_propositional) =
            SelectionContext::create_for_testing(1, 0, Some(vec![(-10, -5)]));
        let mut test_rng = TestRandom::default();
        let mut context = SelectionContext::new(
            &assignments_integer,
            &assignments_propositional,
            &mut test_rng,
        );
        let domain_ids = context.get_domains().collect::<Vec<_>>();

        let mut selector = InDomainMiddle;

        let selected_predicate = selector.select_value(&mut context, domain_ids[0]);
        assert_eq!(selected_predicate, predicate!(domain_ids[0] == -8))
    }
}
