use super::ReifiedRetentionChecker;
use crate::AtomicConstraint;
use crate::CheckerVariable;
use crate::RetentionCheck;
use crate::RetentionChecker;
use crate::VariableState;

impl<Atomic, Var> RetentionChecker<Atomic> for ReifiedRetentionChecker<Atomic, Var>
where
    Atomic: AtomicConstraint,
    Var: CheckerVariable<Atomic>,
{
    fn check_retention(&self, state: &VariableState<Atomic>) -> RetentionCheck {
        if self.reification_literal.induced_domain_contains(state, 0) {
            return RetentionCheck::NothingToPropagate;
        }

        self.inner.check_retention(state)
    }
}
