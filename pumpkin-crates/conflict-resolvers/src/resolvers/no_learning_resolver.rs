use pumpkin_core::conflict_analysis::ConflictAnalysisContext;
use pumpkin_core::conflict_analysis::ConflictResolver;
use pumpkin_core::propagation::ReadDomains;

/// Resolve conflicts by backtracking one decision level trying the opposite of the last decision.
#[derive(Default, Debug, Clone, Copy)]
pub struct NoLearningResolver;

impl ConflictResolver for NoLearningResolver {
    fn resolve_conflict(&mut self, context: &mut ConflictAnalysisContext) {
        let last_decision = context
            .find_last_decision()
            .expect("the solver is not at decision level 0, so there exists a last decision");

        context.backtrack(context.get_checkpoint() - 1);
        context.enqueue_propagated_predicate(!last_decision);
    }
}
