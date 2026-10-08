use std::fmt::Debug;

use super::NogoodChecker;
use crate::AtomicConstraint;
use crate::ConflictChecker;
use crate::DomainView;

impl<Atomic> ConflictChecker<Atomic> for NogoodChecker<Atomic>
where
    Atomic: AtomicConstraint + Clone + Debug,
{
    fn check(&self, state: crate::VariableState<Atomic>, _: &[Atomic], _: Option<&Atomic>) -> bool {
        self.nogood.iter().all(|atomic| state.is_true(atomic))
    }
}
