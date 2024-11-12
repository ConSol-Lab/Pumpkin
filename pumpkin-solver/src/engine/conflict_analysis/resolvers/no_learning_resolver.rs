use super::ConflictResolver;
use crate::engine::conflict_analysis::ConflictAnalysisContext;
use crate::engine::conflict_analysis::LearnedNogood;
use crate::pumpkin_assert_simple;

#[derive(Default, Debug, Clone, Copy)]
pub struct NoLearningResolver;

impl ConflictResolver for NoLearningResolver {
    fn resolve_conflict(
        &mut self,
        _context: &mut ConflictAnalysisContext,
    ) -> Option<LearnedNogood> {
        None
    }

    fn process(
        &mut self,
        context: &mut ConflictAnalysisContext,
        learned_nogood: &Option<LearnedNogood>,
    ) -> Result<(), ()> {
        pumpkin_assert_simple!(learned_nogood.is_none());

        if let Some(last_decision) = context.find_last_decision() {
            context.backtrack(context.assignments.get_decision_level() - 1);
            context.enqueue_propagated_predicate(!last_decision);
            Ok(())
        } else {
            Err(())
        }
    }
}
