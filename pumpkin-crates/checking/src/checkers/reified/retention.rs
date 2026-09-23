use super::ReifiedRetentionChecker;
use crate::AtomicConstraint;
use crate::CheckerVariable;
use crate::RetentionChecker;
use crate::VariableState;

impl<Atomic, Var> RetentionChecker<Atomic> for ReifiedRetentionChecker<Atomic, Var>
where
    Atomic: AtomicConstraint,
    Var: CheckerVariable<Atomic>,
{
    fn check_retention(&self, state: &VariableState<Atomic>) -> bool {
        if self.reification_literal.induced_domain_contains(state, 0) {
            return true;
        }

        self.inner.check_retention(state)
    }
}
