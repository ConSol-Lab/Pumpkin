use super::ConflictAnalysisResult;
use super::ConflictResolver;
use super::ResolutionConflictAnalyser;
use crate::engine::conflict_analysis::ConflictAnalysisContext;
use crate::pumpkin_assert_simple;

#[derive(Default, Debug)]
pub struct NoLearning {
    resolution: ResolutionConflictAnalyser,
}

impl ConflictResolver for NoLearning {
    fn resolve_conflict(
        &mut self,
        context: &mut ConflictAnalysisContext,
    ) -> Option<ConflictAnalysisResult> {
        if context
            .assignments_propositional
            .get_previous_decisions()
            .next()
            .is_some()
        {
            let _ = self.resolution.resolve_conflict(context);
        }
        None
    }

    fn process(
        &mut self,
        context: &mut ConflictAnalysisContext,
        learned_nogood: &Option<ConflictAnalysisResult>,
    ) -> Result<(), ()> {
        pumpkin_assert_simple!(learned_nogood.is_none());
        let last_decision = context.last_decision();

        context.backtrack(context.get_decision_level() - 1);
        context.enqueue_propagated_literal(!last_decision);
        Ok(())
    }
}
