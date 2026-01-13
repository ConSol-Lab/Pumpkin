use std::fmt::Debug;

use dyn_clone::DynClone;
use dyn_clone::clone_trait_object;

use crate::engine::conflict_analysis::ConflictAnalysisContext;
#[cfg(doc)]
use crate::engine::reason::ReasonStore;

clone_trait_object!(ConflictResolver);

/// A [`ConflictResolver`] is responsible for restoring the state of the solver so that search can
/// continue after a conflict is encountered.
///
/// See [`ConflictResolver::resolve_conflict`] for more information.
pub trait ConflictResolver: Debug + DynClone {
    /// Resolve a conflicting state in the solver so that search can proceed.
    ///
    /// The state provided will be inconsistent. The last entry on the trail is the last
    /// _successful_ propagation, and the conflict information contains either an explicit conflict
    /// nogood or the predicate and reason that triggered a domain to become empty. The reason of
    /// any propagation can be queried using [`ConflictAnalysisContext::get_propagation_reason`].
    ///
    /// Additionally, it is guaranteed that the conflict is not at the root-level. Such a conflict
    /// means that nothing could restore the solver, so resolving a root-level conflict is
    /// nonsensical. A consequence of this is that an implementation of
    /// [`ConflictResolver::resolve_conflict`] does not have an error path. All implementations
    /// should succeed, or panic if an unexpected state is encountered.
    ///
    /// For now, we assume that infeasibility is always discovered at the root. Hence, we assume
    /// this function is never called at decision level 0. However, it is conceivable that this
    /// assumption will be relaxed in the future, as expensive propagators may not be called at
    /// every node. We explain this here for completeness only, and it remains safe to assume
    /// decision level will be non-zero.
    ///
    /// When this function exits, the solver must be in a state that it can proceed in the solving
    /// loop. This means it will first perform fixpoint propagation, then branch, and repeat.
    /// Typically, implementations of this function will make an assignment that will prevent
    /// the solver from entering the same subtree again.
    fn resolve_conflict(&mut self, context: &mut ConflictAnalysisContext);
}
