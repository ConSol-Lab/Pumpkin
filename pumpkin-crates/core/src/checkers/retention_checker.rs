use std::fmt::Debug;

use dyn_clone::DynClone;

use crate::checkers::Scope;
use crate::propagation::Domains;

/// A runtime verifier that determines whether domains are sufficiently pruned.
pub trait RetentionChecker: Debug + DynClone {
    /// Ensure the domains do not have values that should have been removed by propagation.
    ///
    /// Returns `true` if the domains are sufficiently pruned, or `false` otherwise.
    fn check_retention(&mut self, scope: &Scope, domains: Domains<'_>) -> bool;
}

/// Wrapper around `Box<dyn RetentionChecker>` that implements [`Clone`].
#[derive(Debug)]
pub struct BoxedRetentionChecker(Box<dyn RetentionChecker>);

impl Clone for BoxedRetentionChecker {
    fn clone(&self) -> Self {
        BoxedRetentionChecker(dyn_clone::clone_box(&*self.0))
    }
}

impl<T> From<T> for BoxedRetentionChecker
where
    T: RetentionChecker + 'static,
{
    fn from(value: T) -> Self {
        BoxedRetentionChecker(Box::new(value))
    }
}

impl BoxedRetentionChecker {
    pub fn check_retention(&mut self, scope: &Scope, domains: Domains<'_>) -> bool {
        self.0.check_retention(scope, domains)
    }
}
