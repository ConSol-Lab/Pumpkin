use pumpkin_checking::AtomicConstraint;
use pumpkin_checking::BoxedChecker;
use pumpkin_checking::CheckerVariable;
use pumpkin_checking::ConflictChecker;

#[derive(Debug, Clone)]
pub struct ReifiedChecker<Atomic: AtomicConstraint, Var> {
    pub inner: BoxedChecker<Atomic>,
    pub reification_literal: Var,
}

impl<Atomic: AtomicConstraint + Clone, Var: CheckerVariable<Atomic>> ConflictChecker<Atomic>
    for ReifiedChecker<Atomic, Var>
{
    fn check(
        &self,
        state: pumpkin_checking::VariableState<Atomic>,
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
