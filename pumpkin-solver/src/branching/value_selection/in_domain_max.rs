use crate::branching::SelectionContext;
use crate::branching::ValueSelector;
use crate::engine::predicates::predicate::Predicate;
use crate::engine::variables::IntegerVariable;
use crate::engine::variables::Literal;
use crate::engine::variables::PropositionalVariable;
use crate::predicate;

/// [`ValueSelector`] which chooses to assign the provided variable to its upper-bound.
#[derive(Debug, Copy, Clone)]
pub struct InDomainMax;

impl<Var: IntegerVariable + Copy> ValueSelector<Var> for InDomainMax {
    fn select_value(
        &mut self,
        context: &mut SelectionContext,
        decision_variable: Var,
    ) -> Predicate {
        predicate!(decision_variable >= context.upper_bound(decision_variable))
    }
}

impl ValueSelector<PropositionalVariable> for InDomainMax {
    fn select_value(
        &mut self,
        _context: &mut SelectionContext,
        decision_variable: PropositionalVariable,
    ) -> Predicate {
        Literal::new(decision_variable, true).into()
    }
}

#[cfg(test)]
mod tests {
    use crate::basic_types::tests::TestRandom;
    use crate::branching::InDomainMax;
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

        let mut selector = InDomainMax;

        let selected_predicate = selector.select_value(&mut context, domain_ids[0]);
        assert_eq!(selected_predicate, predicate!(domain_ids[0] >= 10))
    }
}
