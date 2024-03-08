use crate::basic_types::DomainId;
use crate::basic_types::Literal;
use crate::basic_types::PredicateConstructor;
use crate::branching::SelectionContext;
use crate::branching::ValueSelector;

/// A [`ValueSelector`] which excludes the largest value from the domain.
#[derive(Debug, Copy, Clone)]
pub struct OutDomainMax;

impl ValueSelector<DomainId> for OutDomainMax {
    fn select_value(
        &mut self,
        context: &mut SelectionContext,
        decision_variable: DomainId,
    ) -> Literal {
        context.get_literal_for_predicate(
            decision_variable.upper_bound_predicate(context.upper_bound(decision_variable) - 1),
        )
    }
}

#[cfg(test)]
mod tests {
    use crate::basic_types::tests::TestRandom;
    use crate::basic_types::PredicateConstructor;
    use crate::branching::OutDomainMax;
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

        let mut selector = OutDomainMax;

        let selected_literal = selector.select_value(&mut context, domain_ids[0]);
        assert_eq!(
            selected_literal,
            mediator.get_predicate_literal(
                domain_ids[0].upper_bound_predicate(9),
                &assignments_integer
            )
        )
    }
}
