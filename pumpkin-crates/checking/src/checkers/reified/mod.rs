mod conflict;

use crate::AtomicConstraint;
use crate::BoxedConflictChecker;

/// The conflict checker of a rule that only has to hold when the reification literal is true.
#[derive(Debug, Clone)]
pub struct ReifiedChecker<Atomic: AtomicConstraint, Var> {
    pub inner: BoxedConflictChecker<Atomic>,
    pub reification_literal: Var,
}
