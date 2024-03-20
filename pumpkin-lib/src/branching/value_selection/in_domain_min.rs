use super::ValueSelector;
use crate::branching::SelectionContext;
use crate::engine::variables::IntegerVariable;
use crate::engine::variables::Literal;
use crate::engine::variables::PropositionalVariable;

/// [`ValueSelector`] which chooses to assign the provided variable to its lowest-bound.
#[derive(Debug, Copy, Clone)]
pub struct InDomainMin;

impl<Var: IntegerVariable + Copy> ValueSelector<Var> for InDomainMin {
    fn select_value(&mut self, context: &mut SelectionContext, decision_variable: Var) -> Literal {
        context.get_literal_for_predicate(
            decision_variable.upper_bound_predicate(context.lower_bound(decision_variable)),
        )
    }
}

impl ValueSelector<PropositionalVariable> for InDomainMin {
    fn select_value(
        &mut self,
        _context: &mut SelectionContext,
        decision_variable: PropositionalVariable,
    ) -> Literal {
        Literal::new(decision_variable, false)
    }
}

#[cfg(test)]
mod tests {
    use crate::basic_types::tests::TestRandom;
    use crate::basic_types::PredicateConstructor;
    use crate::branching::InDomainMin;
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

        let mut selector = InDomainMin;

        let selected_literal = selector.select_value(&mut context, domain_ids[0]);
        assert_eq!(
            selected_literal,
            mediator
                .get_predicate_literal(domain_ids[0].equality_predicate(0), &assignments_integer)
        )
    }
}
