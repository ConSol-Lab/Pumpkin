use std::fmt::Debug;

use super::NogoodChecker;
use crate::AtomicConstraint;
use crate::ConflictCheck;
use crate::ConflictChecker;
use crate::DomainView;
use crate::VariableState;

impl<Atomic> ConflictChecker<Atomic> for NogoodChecker<Atomic>
where
    Atomic: AtomicConstraint + Clone + Debug,
{
    fn check(
        &self,
        state: &VariableState<Atomic>,
        _: &[Atomic],
        _: Option<&Atomic>,
    ) -> ConflictCheck {
        ConflictCheck::detected_if(self.nogood.iter().all(|atomic| state.is_true(atomic)))
    }
}
