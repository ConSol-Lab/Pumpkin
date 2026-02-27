use std::fmt::Debug;

use dyn_clone::DynClone;

use crate::propagation::Domains;
use crate::propagation::checkers::Scope;

/// A consistency checker ensures that a propagator is at a certain level of consistency.
///
/// Given a current set of domains and a scope, the checker identifies whether the desired
/// consistency level is reached.
pub trait ConsistencyChecker: Debug + DynClone {
    /// Returns `true` if the variables in `scope` are at the required consistency in
    /// `domains`.
    fn check_consistency(&self, domains: Domains<'_>, scope: &Scope) -> bool;
}

/// Wrapper around `Box<dyn ConsistencyChecker>` that implements [`Clone`].
#[derive(Debug)]
pub struct BoxedConsistencyChecker(Box<dyn ConsistencyChecker>);

impl Clone for BoxedConsistencyChecker {
    fn clone(&self) -> Self {
        BoxedConsistencyChecker(dyn_clone::clone_box(&*self.0))
    }
}

impl From<Box<dyn ConsistencyChecker>> for BoxedConsistencyChecker {
    fn from(value: Box<dyn ConsistencyChecker>) -> Self {
        BoxedConsistencyChecker(value)
    }
}

impl BoxedConsistencyChecker {
    /// See [`ConsistencyChecker::check_consistency`].
    pub fn check_consistency(&self, domains: Domains<'_>, scope: &Scope) -> bool {
        self.0.check_consistency(domains, scope)
    }
}
