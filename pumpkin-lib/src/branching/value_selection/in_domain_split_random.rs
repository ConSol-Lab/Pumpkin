use crate::basic_types::DomainId;
use crate::basic_types::Literal;
use crate::basic_types::PredicateConstructor;
use crate::branching::SelectionContext;
use crate::branching::ValueSelector;

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
    ) -> Literal {
        let bound = context.lower_bound(decision_variable)
            + (context.get_size_of_domain(decision_variable) as f64 / 2.0).floor() as i32;

        if context.random().generate_bool(0.5) {
            context.get_literal_for_predicate(decision_variable.lower_bound_predicate(bound))
        } else {
            context.get_literal_for_predicate(decision_variable.upper_bound_predicate(bound))
        }
    }
}

#[cfg(test)]
mod tests {

    use crate::basic_types::tests::TestRandom;
    use crate::basic_types::PredicateConstructor;
    use crate::branching::InDomainSplitRandom;
    use crate::branching::SelectionContext;
    use crate::branching::ValueSelector;

    #[test]
    fn test_returns_correct_literal() {
        let (assignments_integer, assignments_propositional, mediator) =
            SelectionContext::create_for_testing(1, 0, Some(vec![(0, 10)]));
        let mut test_random = TestRandom {
            usizes: vec![5],
            bools: vec![true],
        };
        let mut context = SelectionContext::new(
            &assignments_integer,
            &assignments_propositional,
            &mediator,
            &mut test_random,
        );
        let domain_ids = context.get_domains().collect::<Vec<_>>();

        let mut selector = InDomainSplitRandom;

        let selected_literal = selector.select_value(&mut context, domain_ids[0]);

        assert_eq!(
            selected_literal,
            mediator.get_predicate_literal(
                domain_ids[0].lower_bound_predicate(5),
                &assignments_integer
            )
        )
    }
}
