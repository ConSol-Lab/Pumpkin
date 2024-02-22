use super::ValueSelector;
use crate::basic_types::variables::IntVar;
use crate::basic_types::Literal;
use crate::branching::SelectionContext;

/// [`ValueSelector`] which chooses to assign the provided variable to its lowest-bound.
#[derive(Debug, Copy, Clone)]
pub struct InDomainMin;

impl<Var: IntVar + Copy> ValueSelector<Var> for InDomainMin {
    fn select_value(&mut self, context: &SelectionContext, decision_variable: Var) -> Literal {
        context.get_literal_for_predicate::<Var>(
            decision_variable.equality_predicate(context.lower_bound(decision_variable)),
        )
    }
}
