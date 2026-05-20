mod bounds_consistency;
mod scope;
mod store;
pub mod support;

use std::fmt::Debug;

pub use bounds_consistency::*;
use dyn_clone::DynClone;
pub use scope::*;
pub use store::*;

use crate::propagation::Domains;

pub trait ConsistencyChecker: Debug + DynClone {
    /// Ensure the domains of all items in the scope are at the advertised consistency level.
    ///
    /// Returns `true` if the consistency check passes, or `false` otherwise.
    fn check_consistency(&mut self, scope: &Scope, domains: Domains<'_>) -> bool;
}

/// Wrapper around `Box<dyn ConsistencyChecker>` that implements [`Clone`].
#[derive(Debug)]
pub struct BoxedConsistencyChecker(Box<dyn ConsistencyChecker>);

impl Clone for BoxedConsistencyChecker {
    fn clone(&self) -> Self {
        BoxedConsistencyChecker(dyn_clone::clone_box(&*self.0))
    }
}

impl<T> From<T> for BoxedConsistencyChecker
where
    T: ConsistencyChecker + 'static,
{
    fn from(value: T) -> Self {
        BoxedConsistencyChecker(Box::new(value))
    }
}

impl BoxedConsistencyChecker {
    pub fn check_consistency(&mut self, scope: &Scope, domains: Domains<'_>) -> bool {
        self.0.check_consistency(scope, domains)
    }
}
