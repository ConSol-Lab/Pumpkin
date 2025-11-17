use log::trace;

use super::ConflictResolver;
use crate::engine::conflict_analysis::ConflictAnalysisContext;

/// Resolve conflicts by backtracking one decision level trying the opposite of the last decision.
#[derive(Default, Debug, Clone, Copy)]
pub(crate) struct NoLearningResolver;

impl ConflictResolver for NoLearningResolver {
    fn resolve_conflict(&mut self, mut context: ConflictAnalysisContext) -> bool {
        let last_decision = context
            .find_last_decision()
            .expect("resolve_conflict is never called at the root");

        let dl = context.assignments.get_decision_level() - 1;
        trace!("backtracking to dl {dl}");
        context.backtrack(dl);
        context.enqueue_propagated_predicate(!last_decision);
        true
    }
}
