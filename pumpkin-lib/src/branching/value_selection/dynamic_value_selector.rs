use std::fmt::Debug;

use super::ValueSelector;
use crate::basic_types::SolutionReference;
#[cfg(doc)]
use crate::branching::branchers::dynamic_brancher::DynamicBrancher;
use crate::branching::SelectionContext;
use crate::engine::predicates::predicate::Predicate;
use crate::engine::variables::DomainId;
use crate::engine::variables::Literal;

/// Similar to [`DynamicBrancher`], this is a pass-along structure which should be used when a
/// [`Sized`] object is required.
pub struct DynamicValueSelector<Var> {
    selector: Box<dyn ValueSelector<Var>>,
}

impl<Var> Debug for DynamicValueSelector<Var> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("DynamicValueSelector").finish()
    }
}

impl<Var> DynamicValueSelector<Var> {
    pub fn new(selector: Box<dyn ValueSelector<Var>>) -> Self {
        Self { selector }
    }
}

impl<Var> ValueSelector<Var> for DynamicValueSelector<Var> {
    fn select_value(
        &mut self,
        context: &mut SelectionContext,
        decision_variable: Var,
    ) -> Predicate {
        self.selector.select_value(context, decision_variable)
    }

    fn on_solution(&mut self, solution: SolutionReference) {
        self.selector.on_solution(solution)
    }

    fn on_unassign_integer(&mut self, variable: DomainId, value: i32) {
        self.selector.on_unassign_integer(variable, value)
    }

    fn on_unassign_literal(&mut self, literal: Literal) {
        self.selector.on_unassign_literal(literal)
    }

    fn is_static(&self) -> bool {
        self.selector.is_static()
    }
}
