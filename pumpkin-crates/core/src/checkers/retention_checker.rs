use std::fmt::Debug;

use dyn_clone::DynClone;

use crate::checkers::Scope;
use crate::propagation::Domains;

/// A runtime verifier that determines whether a propagator has nothing left to propagate.
///
/// The contract mirrors the retention conditions of the formally verified proof checker: in the
/// current domains no inference of the propagator's rule applies, i.e. giving any variable in
/// the scope any value of its domain does not let the rule report a conflict. Each propagator
/// supplies its own checker, which may exploit the structure of its rule to decide this cheaply.
pub trait RetentionChecker: Debug + DynClone {
    /// Returns `true` if the propagator has nothing left to propagate in `domains`, and `false`
    /// if some inference of its rule still applies.
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
