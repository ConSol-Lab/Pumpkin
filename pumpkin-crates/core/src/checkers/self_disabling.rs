use std::sync::Arc;
use std::sync::atomic::AtomicBool;
use std::sync::atomic::Ordering;

use super::BoxedConsistencyChecker;
use super::ConsistencyChecker;
use super::Scope;
use crate::propagation::Domains;

/// A [`ConsistencyChecker`] wrapper that skips the inner check when the associated constraint has
/// been deleted.
///
/// The deletion flag is shared with the constraint owner (e.g. the nogood propagator). Setting the
/// flag to `true` causes the checker to become a permanent no-op.
#[derive(Debug, Clone)]
pub struct SelfDisablingChecker {
    pub inner: BoxedConsistencyChecker,
    pub is_deleted: Arc<AtomicBool>,
}

impl ConsistencyChecker for SelfDisablingChecker {
    fn check_consistency(&mut self, scope: &Scope, domains: Domains<'_>) -> bool {
        if self.is_deleted.load(Ordering::Relaxed) {
            return true;
        }
        self.inner.check_consistency(scope, domains)
    }
}
