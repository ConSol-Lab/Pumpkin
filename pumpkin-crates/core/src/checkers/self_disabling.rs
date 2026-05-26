use std::sync::Arc;
use std::sync::atomic::AtomicBool;
use std::sync::atomic::Ordering;

use super::RetentionChecker;
use super::Scope;
use crate::propagation::Domains;

/// A [`RetentionChecker`] wrapper that skips the inner check when the associated constraint has
/// been deleted.
///
/// The deletion flag is shared with the constraint owner (e.g. the nogood propagator). Setting the
/// flag to `true` causes the checker to become a permanent no-op.
#[derive(Debug, Clone)]
pub struct SelfDisablingChecker<T> {
    inner: T,
    is_deleted: Arc<AtomicBool>,
}

impl<T> SelfDisablingChecker<T> {
    /// Create a new self-disabling checker.
    ///
    /// The deletion flag can be obtained with [`SelfDisablingChecker::deletion_flag`].
    pub fn new(checker: T) -> Self {
        SelfDisablingChecker {
            inner: checker,
            is_deleted: Arc::new(AtomicBool::new(false)),
        }
    }

    /// The deletion flag for this self-disabling checker.
    pub fn deletion_flag(&self) -> Arc<AtomicBool> {
        Arc::clone(&self.is_deleted)
    }
}

impl<T: RetentionChecker + Clone> RetentionChecker for SelfDisablingChecker<T> {
    fn check_retention(&mut self, scope: &Scope, domains: Domains<'_>) -> bool {
        if self.is_deleted.load(Ordering::Relaxed) {
            return true;
        }
        self.inner.check_retention(scope, domains)
    }
}
