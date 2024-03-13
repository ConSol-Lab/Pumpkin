use crate::basic_types::variables::IntVar;
use crate::basic_types::Literal;
use crate::branching::SelectionContext;
use crate::branching::ValueSelector;

/// A [`ValueSelector`] which selects the median value in the domain (or if this value is already
/// assigned then the closest variable to it in terms of index).
#[derive(Debug, Copy, Clone)]
pub struct InDomainMedian;

impl<Var: IntVar + Copy> ValueSelector<Var> for InDomainMedian {
    fn select_value(&mut self, context: &mut SelectionContext, decision_variable: Var) -> Literal {
        let values_in_domain = (context.lower_bound(decision_variable)
            ..=context.upper_bound(decision_variable))
            .filter(|bound| context.contains(decision_variable, *bound))
            .collect::<Vec<_>>();
        context.get_literal_for_predicate(
            decision_variable.equality_predicate(values_in_domain[values_in_domain.len() / 2]),
        )
    }
}

#[cfg(test)]
mod tests {
    use crate::basic_types::tests::TestRandom;
    use crate::basic_types::PredicateConstructor;
    use crate::branching::InDomainMedian;
    use crate::branching::SelectionContext;
    use crate::branching::ValueSelector;

    #[test]
    fn test_returns_correct_literal() {
        let (assignments_integer, assignments_propositional, mediator) =
            SelectionContext::create_for_testing(1, 0, Some(vec![(0, 10)]));
        let mut test_rng = TestRandom::default();
        let mut context = SelectionContext::new(
            &assignments_integer,
            &assignments_propositional,
            &mediator,
            &mut test_rng,
        );
        let domain_ids = context.get_domains().collect::<Vec<_>>();

        let mut selector = InDomainMedian;

        let selected_literal = selector.select_value(&mut context, domain_ids[0]);
        assert_eq!(
            selected_literal,
            mediator
                .get_predicate_literal(domain_ids[0].equality_predicate(5), &assignments_integer)
        )
    }

    #[test]
    fn test_returns_correct_literal_no_median() {
        let (mut assignments_integer, assignments_propositional, mediator) =
            SelectionContext::create_for_testing(1, 0, Some(vec![(1, 10)]));
        let mut test_rng = TestRandom::default();

        let domain_ids = assignments_integer.get_domains().collect::<Vec<_>>();
        let mut selector = InDomainMedian;

        let _ = assignments_integer.remove_value_from_domain(domain_ids[0], 9, None);

        let mut context = SelectionContext::new(
            &assignments_integer,
            &assignments_propositional,
            &mediator,
            &mut test_rng,
        );

        let selected_literal = selector.select_value(&mut context, domain_ids[0]);
        assert_eq!(
            selected_literal,
            mediator
                .get_predicate_literal(domain_ids[0].equality_predicate(5), &assignments_integer)
        )
    }

    #[test]
    fn test_returns_correct_literal_removed_median() {
        let (mut assignments_integer, assignments_propositional, mediator) =
            SelectionContext::create_for_testing(1, 0, Some(vec![(1, 10)]));
        let mut test_rng = TestRandom::default();

        let domain_ids = assignments_integer.get_domains().collect::<Vec<_>>();
        let mut selector = InDomainMedian;

        let _ = assignments_integer.remove_value_from_domain(domain_ids[0], 5, None);

        let mut context = SelectionContext::new(
            &assignments_integer,
            &assignments_propositional,
            &mediator,
            &mut test_rng,
        );

        let selected_literal = selector.select_value(&mut context, domain_ids[0]);
        assert_eq!(
            selected_literal,
            mediator
                .get_predicate_literal(domain_ids[0].equality_predicate(6), &assignments_integer)
        )
    }
}
