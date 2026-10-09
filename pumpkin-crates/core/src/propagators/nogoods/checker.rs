use std::fmt::Debug;

use pumpkin_checking::AtomicConstraint;
use pumpkin_checking::ConflictChecker;
use pumpkin_checking::DomainView;

#[derive(Debug, Clone)]
pub struct NogoodChecker<Atomic> {
    pub nogood: Box<[Atomic]>,
}

impl<Atomic> ConflictChecker<Atomic> for NogoodChecker<Atomic>
where
    Atomic: AtomicConstraint + Clone + Debug,
{
    fn check(
        &self,
        state: pumpkin_checking::VariableState<Atomic>,
        _: &[Atomic],
        _: Option<&Atomic>,
    ) -> bool {
        self.nogood.iter().all(|atomic| state.is_true(atomic))
    }
}
