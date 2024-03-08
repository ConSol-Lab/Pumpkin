use super::InDomainSplit;
use crate::basic_types::DomainId;
use crate::basic_types::Literal;
use crate::basic_types::PredicateConstructor;
use crate::branching::SelectionContext;
use crate::branching::ValueSelector;

/// If the domain consists of several intervals (e.g. a variable with the domain {0, 1, 4, 5, 6, 9,
/// 10} consists of the interval {[0-1], [4-6], [9-10]}), then this [`ValueSelector`] will reduce
/// the domain to the first interval (e.g. to {0, 1} in the previous example). Otherwise (i.e. if
/// the domain is one continuous interval) then it will bisect the domain in the same manner as
/// [`InDomainSplit`].
#[derive(Debug, Copy, Clone)]
pub struct InDomainInterval;

impl ValueSelector<DomainId> for InDomainInterval {
    fn select_value(
        &mut self,
        context: &mut SelectionContext,
        decision_variable: DomainId,
    ) -> Literal {
        // We attempt to find the first hole in the domain (i.e. the value is not in the domain of
        // `decision_variable`) Note that the lower-bound and upper-bound are guaranteed to
        // be in the domain
        if let Some(first_interval) = (context.lower_bound(decision_variable) + 1
            ..context.upper_bound(decision_variable))
            .find(|bound| !context.contains(decision_variable, *bound))
        {
            // We use `first_interval - 1` since `first_interval` is the value of the first value
            // which is not in the domain (and the upper-bound predicate is inclusive)
            context.get_literal_for_predicate(
                decision_variable.upper_bound_predicate(first_interval - 1),
            )
        } else {
            // There are no holes in the domain, we bisect the domain and exclude the upper-half of
            // the domain
            InDomainSplit::get_literal_excluding_upper_half(context, decision_variable)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::InDomainInterval;
    use crate::basic_types::tests::TestRandom;
    use crate::basic_types::PredicateConstructor;
    use crate::branching::SelectionContext;
    use crate::branching::ValueSelector;

    #[test]
    fn test_returns_correct_literal() {
        let (mut assignments_integer, assignments_propositional, mediator) =
            SelectionContext::create_for_testing(1, 0, Some(vec![(0, 10)]));
        let mut test_rng = TestRandom::default();
        let domain_ids = assignments_integer.get_domains().collect::<Vec<_>>();
        let mut selector = InDomainInterval;

        for to_remove in [2, 3, 7, 8] {
            let _ = assignments_integer.remove_value_from_domain(domain_ids[0], to_remove, None);
        }

        let mut context = SelectionContext::new(
            &assignments_integer,
            &assignments_propositional,
            &mediator,
            &mut test_rng,
        );

        let selected_literal = selector.select_value(&mut context, domain_ids[0]);
        assert_eq!(
            selected_literal,
            mediator.get_predicate_literal(
                domain_ids[0].upper_bound_predicate(1),
                &assignments_integer
            )
        )
    }

    #[test]
    fn test_no_holes_in_domain_bisects_domain() {
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

        let mut selector = InDomainInterval;

        let selected_literal = selector.select_value(&mut context, domain_ids[0]);

        assert_eq!(
            selected_literal,
            mediator.get_predicate_literal(
                domain_ids[0].upper_bound_predicate(5),
                &assignments_integer
            )
        )
    }

    #[test]
    fn test_domain_of_size_two() {
        let (assignments_integer, assignments_propositional, mediator) =
            SelectionContext::create_for_testing(1, 0, Some(vec![(1, 2)]));
        let mut test_rng = TestRandom::default();
        let mut context = SelectionContext::new(
            &assignments_integer,
            &assignments_propositional,
            &mediator,
            &mut test_rng,
        );
        let domain_ids = context.get_domains().collect::<Vec<_>>();

        let mut selector = InDomainInterval;

        let selected_literal = selector.select_value(&mut context, domain_ids[0]);

        assert_eq!(
            selected_literal,
            mediator.get_predicate_literal(
                domain_ids[0].upper_bound_predicate(1),
                &assignments_integer
            )
        )
    }
}
