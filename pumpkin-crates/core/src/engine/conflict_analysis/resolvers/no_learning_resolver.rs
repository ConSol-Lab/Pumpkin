use super::ConflictResolver;
use crate::engine::conflict_analysis::ConflictAnalysisContext;

/// Resolve conflicts by backtracking one decision level trying the opposite of the last decision.
#[derive(Default, Debug, Clone, Copy)]
pub(crate) struct NoLearningResolver;

impl ConflictResolver for NoLearningResolver {
    fn resolve_conflict(&mut self, context: &mut ConflictAnalysisContext) {
        let last_decision = context
            .find_last_decision()
            .expect("the solver is not at decision level 0, so there exists a last decision");

        context.backtrack(context.assignments.get_decision_level() - 1);
        context.enqueue_propagated_predicate(!last_decision);
    }
}
