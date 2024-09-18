use super::ConflictAnalysisResult;
use super::ConflictResolver;
use crate::engine::conflict_analysis::ConflictAnalysisContext;
use crate::pumpkin_assert_simple;

#[derive(Default, Debug, Clone, Copy)]
pub struct NoLearning;

impl ConflictResolver for NoLearning {
    fn resolve_conflict(
        &mut self,
        _context: &mut ConflictAnalysisContext,
    ) -> Option<ConflictAnalysisResult> {
        None
    }

    fn process(
        &mut self,
        context: &mut ConflictAnalysisContext,
        learned_nogood: &Option<ConflictAnalysisResult>,
    ) -> Result<(), ()> {
        pumpkin_assert_simple!(learned_nogood.is_none());

        if let Some(last_decision) = context.last_decision() {
            context.backtrack(context.get_decision_level() - 1);
            context.enqueue_propagated_literal(!last_decision);
            Ok(())
        } else {
            Err(())
        }
    }
}
