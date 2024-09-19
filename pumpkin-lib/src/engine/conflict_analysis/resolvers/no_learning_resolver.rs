use super::ConflictResolver;
use super::ResolutionResolver;
use crate::engine::conflict_analysis::ConflictAnalysisNogoodContext;
use crate::engine::conflict_analysis::LearnedNogood;
use crate::pumpkin_assert_simple;

#[derive(Default, Debug)]
pub struct NoLearningResolver {
    analyser: ResolutionResolver,
}

impl ConflictResolver for NoLearningResolver {
    fn resolve_conflict(
        &mut self,
        context: &mut ConflictAnalysisNogoodContext,
    ) -> Option<LearnedNogood> {
        if context.assignments.decisions.len() > 1 {
            let _ = self.analyser.resolve_conflict(context);
        }
        None
    }

    fn process(
        &mut self,
        context: &mut ConflictAnalysisNogoodContext,
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
