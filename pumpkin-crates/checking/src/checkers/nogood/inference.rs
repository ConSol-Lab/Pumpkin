use std::fmt::Debug;

use super::NogoodChecker;
use crate::AtomicConstraint;
use crate::InferenceChecker;
use crate::VariableState;

impl<Atomic> InferenceChecker<Atomic> for NogoodChecker<Atomic>
where
    Atomic: AtomicConstraint + Clone + Debug,
{
    fn check(&self, state: VariableState<Atomic>, _: &[Atomic], _: Option<&Atomic>) -> bool {
        self.nogood.iter().all(|atomic| state.is_true(atomic))
    }
}
