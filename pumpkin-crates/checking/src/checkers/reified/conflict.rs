use super::ReifiedChecker;
use crate::AtomicConstraint;
use crate::CheckerVariable;
use crate::ConflictChecker;

impl<Atomic: AtomicConstraint + Clone, Var: CheckerVariable<Atomic>> ConflictChecker<Atomic>
    for ReifiedChecker<Atomic, Var>
{
    fn check(
        &self,
        state: crate::VariableState<Atomic>,
        premises: &[Atomic],
        consequent: Option<&Atomic>,
    ) -> bool {
        if self.reification_literal.induced_domain_contains(&state, 0) {
            return false;
        }

        if let Some(consequent) = consequent
            && self
                .reification_literal
                .does_atomic_constrain_self(consequent)
        {
            self.inner.check(state, premises, None)
        } else {
            self.inner.check(state, premises, consequent)
        }
    }
}
