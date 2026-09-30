use std::fmt::Debug;

use super::NogoodChecker;
use crate::AtomicConstraint;
use crate::InferenceChecker;
use crate::VariableState;

impl<Atomic> NogoodChecker<Atomic> {
    /// The name of the rule of nogoods, also used for nogoods that have no checker.
    pub const RULE_NAME: &'static str = "nogood";
}

impl<Atomic> InferenceChecker<Atomic> for NogoodChecker<Atomic>
where
    Atomic: AtomicConstraint + Clone + Debug,
{
    fn rule_name(&self) -> &'static str {
        Self::RULE_NAME
    }

    fn check(&self, state: VariableState<Atomic>, _: &[Atomic], _: Option<&Atomic>) -> bool {
        self.nogood.iter().all(|atomic| state.is_true(atomic))
    }
}
