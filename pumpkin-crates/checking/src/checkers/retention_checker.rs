use std::fmt::Debug;

use dyn_clone::DynClone;

use crate::AtomicConstraint;
use crate::CheckerVariable;
use crate::VariableState;

/// Verifies that a propagator has nothing left to propagate.
///
/// The state holds the domains of the variables in the checker's scope, and each of them is
/// bounded. The check succeeds when no inference of the rule applies in that state: giving any
/// variable any value of its domain does not let the rule report a conflict.
pub trait RetentionChecker<Atomic: AtomicConstraint>: Debug + DynClone {
    /// Returns `true` if nothing is left to propagate in `state`, and `false` if some inference
    /// of the rule still applies.
    fn check_retention(&self, state: &VariableState<Atomic>) -> bool;
}

/// Wrapper around `Box<dyn RetentionChecker<Atomic>>` that implements [`Clone`].
#[derive(Debug)]
pub struct BoxedRetentionChecker<Atomic: AtomicConstraint>(Box<dyn RetentionChecker<Atomic>>);

impl<Atomic: AtomicConstraint> Clone for BoxedRetentionChecker<Atomic> {
    fn clone(&self) -> Self {
        BoxedRetentionChecker(dyn_clone::clone_box(&*self.0))
    }
}

impl<Atomic: AtomicConstraint> BoxedRetentionChecker<Atomic> {
    pub fn new(checker: impl RetentionChecker<Atomic> + 'static) -> Self {
        BoxedRetentionChecker(Box::new(checker))
    }

    pub fn check_retention(&self, state: &VariableState<Atomic>) -> bool {
        self.0.check_retention(state)
    }
}

/// The lower bound of a variable in the scope of a retention check.
pub(crate) fn scope_lower_bound<Atomic: AtomicConstraint, Var: CheckerVariable<Atomic>>(
    variable: &Var,
    state: &VariableState<Atomic>,
) -> i32 {
    variable
        .induced_lower_bound(state)
        .as_int()
        .unwrap_or_else(|| panic!("{variable:?} is not bounded below in the retention check"))
}

/// The upper bound of a variable in the scope of a retention check.
pub(crate) fn scope_upper_bound<Atomic: AtomicConstraint, Var: CheckerVariable<Atomic>>(
    variable: &Var,
    state: &VariableState<Atomic>,
) -> i32 {
    variable
        .induced_upper_bound(state)
        .as_int()
        .unwrap_or_else(|| panic!("{variable:?} is not bounded above in the retention check"))
}
