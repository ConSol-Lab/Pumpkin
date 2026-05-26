use pumpkin_checking::AtomicConstraint;
use pumpkin_checking::BoxedChecker;
use pumpkin_checking::CheckerVariable;
use pumpkin_checking::InferenceChecker;
use pumpkin_checking::VariableState;

use crate::checkers::BoxedRetentionChecker;
use crate::checkers::RetentionChecker;
use crate::checkers::Scope;
use crate::propagation::Domains;
use crate::propagation::LocalId;
use crate::propagation::ReadDomains;
use crate::variables::Literal;

/// A [`RetentionChecker`] wrapper that skips the inner check when the reification literal is
/// not assigned to true.
#[derive(Debug, Clone)]
pub struct ReifiedRetentionChecker {
    pub inner: BoxedRetentionChecker,
    pub reification_literal: Literal,
    /// The [`LocalId`] of the reification literal in the scope, used to strip it before passing
    /// the scope to the inner checker.
    pub reification_literal_id: LocalId,
}

impl RetentionChecker for ReifiedRetentionChecker {
    fn check_retention(&mut self, scope: &Scope, domains: Domains<'_>) -> bool {
        if domains.evaluate_literal(self.reification_literal) != Some(true) {
            return true;
        }

        let inner_scope = scope.without(self.reification_literal_id);
        self.inner.check_retention(&inner_scope, domains)
    }
}

#[derive(Debug, Clone)]
pub struct ReifiedChecker<Atomic: AtomicConstraint, Var> {
    pub inner: BoxedChecker<Atomic>,
    pub reification_literal: Var,
}

impl<Atomic, Var> InferenceChecker<Atomic> for ReifiedChecker<Atomic, Var>
where
    Atomic: AtomicConstraint + Clone,
    Var: CheckerVariable<Atomic>,
{
    fn check(
        &self,
        state: VariableState<Atomic>,
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
