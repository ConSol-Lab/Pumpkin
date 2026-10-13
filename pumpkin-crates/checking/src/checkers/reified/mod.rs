mod conflict;
mod retention;

use crate::AtomicConstraint;
use crate::BoxedConflictChecker;
use crate::BoxedRetentionChecker;

/// The conflict checker of a rule that only has to hold when the reification literal is true.
#[derive(Debug, Clone)]
pub struct ReifiedChecker<Atomic: AtomicConstraint, Var> {
    pub inner: BoxedConflictChecker<Atomic>,
    pub reification_literal: Var,
}

/// A [`RetentionChecker`](crate::RetentionChecker) wrapper that skips the inner check
/// when the reification literal is not assigned to true.
#[derive(Debug, Clone)]
pub struct ReifiedRetentionChecker<Atomic: AtomicConstraint, Var> {
    pub inner: BoxedRetentionChecker<Atomic>,
    pub reification_literal: Var,
}
