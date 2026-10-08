mod conflict;

use crate::AtomicConstraint;
use crate::BoxedChecker;

#[derive(Debug, Clone)]
pub struct ReifiedChecker<Atomic: AtomicConstraint, Var> {
    pub inner: BoxedChecker<Atomic>,
    pub reification_literal: Var,
}
