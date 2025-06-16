use crate::branching::brancher::BrancherEvent;
use crate::branching::value_selection::ValueSelector;
use crate::branching::SelectionContext;
use crate::engine::predicates::predicate::Predicate;
use crate::predicate;
use crate::variables::IntegerVariable;

/// A [`ValueSelector`] which assigns to a random value in the domain.
#[derive(Debug, Clone, Copy)]
pub struct InDomainRandom;

impl<Var> ValueSelector<Var> for InDomainRandom
where
    Var: IntegerVariable,
{
    fn select_value(
        &mut self,
        context: &mut SelectionContext,
        decision_variable: Var,
    ) -> Predicate {
        let values_in_domain = (context.lower_bound(decision_variable.clone())
            ..=context.upper_bound(decision_variable.clone()))
            .filter(|bound| context.contains(decision_variable.clone(), *bound))
            .collect::<Vec<_>>();
        let random_index = context
            .random()
            .generate_usize_in_range(0..values_in_domain.len());
        predicate!(decision_variable == values_in_domain[random_index])
    }

    fn is_restart_pointless(&mut self) -> bool {
        false
    }

    fn subscribe_to_events(&self) -> Vec<BrancherEvent> {
        vec![]
    }
}

#[cfg(test)]
mod tests {
    use crate::basic_types::tests::TestRandom;
    use crate::branching::value_selection::InDomainRandom;
    use crate::branching::value_selection::ValueSelector;
    use crate::branching::SelectionContext;
    use crate::predicate;

    #[test]
    fn test_returns_correct_literal() {
        let (assignments, _) = SelectionContext::create_for_testing(vec![(0, 10)]);
        let mut test_random = TestRandom {
            usizes: vec![3],
            ..Default::default()
        };
        let mut context = SelectionContext::new(&assignments, &mut test_random);
        let domain_ids = context.get_domains().collect::<Vec<_>>();

        let mut selector = InDomainRandom;

        let selected_predicate = selector.select_value(&mut context, domain_ids[0]);

        assert_eq!(selected_predicate, predicate!(domain_ids[0] == 3))
    }
}
