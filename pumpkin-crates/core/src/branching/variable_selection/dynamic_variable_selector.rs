use std::fmt::Debug;

use super::VariableSelector;
use crate::branching::brancher::BrancherEvent;
#[cfg(doc)]
use crate::branching::branchers::dynamic_brancher::DynamicBrancher;
use crate::branching::SelectionContext;
use crate::engine::predicates::predicate::Predicate;
use crate::engine::variables::DomainId;

/// Similar to [`DynamicBrancher`], this is a pass-along structure which should be used when a
/// [`Sized`] object is required.
pub struct DynamicVariableSelector<Var> {
    selector: Box<dyn VariableSelector<Var>>,
}

impl<Var> Debug for DynamicVariableSelector<Var> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("DynamicVariableSelector").finish()
    }
}

impl<Var> DynamicVariableSelector<Var> {
    pub fn new(selector: Box<dyn VariableSelector<Var>>) -> Self {
        Self { selector }
    }
}

impl<Var> VariableSelector<Var> for DynamicVariableSelector<Var> {
    fn select_variable(&mut self, context: &mut SelectionContext) -> Option<Var> {
        self.selector.select_variable(context)
    }

    fn on_appearance_in_conflict_predicate(&mut self, predicate: Predicate) {
        self.selector.on_appearance_in_conflict_predicate(predicate)
    }

    fn on_conflict(&mut self) {
        self.selector.on_conflict()
    }

    fn on_unassign_integer(&mut self, variable: DomainId, value: i32) {
        self.selector.on_unassign_integer(variable, value)
    }

    fn is_restart_pointless(&mut self) -> bool {
        self.selector.is_restart_pointless()
    }

    fn subscribe_to_events(&self) -> Vec<BrancherEvent> {
        self.selector.subscribe_to_events()
    }
}
