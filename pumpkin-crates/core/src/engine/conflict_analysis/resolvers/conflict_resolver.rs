use std::fmt::Debug;

use crate::engine::conflict_analysis::ConflictAnalysisContext;

pub(crate) trait ConflictResolver: Debug {
    /// Restore the solver such that search can continue. If the solver cannot be repaired, then
    /// false is returned. Otherwise, true is returned.
    fn resolve_conflict(&mut self, context: ConflictAnalysisContext) -> bool;
}
