use super::ConflictResolver;
use super::ResolutionResolver;
use crate::basic_types::moving_averages::MovingAverage;
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
            let learned_nogood = self
                .analyser
                .resolve_conflict(context)
                .expect("Expected resolution conflict to be able to find a nogood");

            let lbd = context
                .lbd_helper
                .compute_lbd(&learned_nogood.predicates, context.assignments);
            context.counters.average_lbd.add_term(lbd.into());
            context
                .counters
                .average_learned_clause_length
                .add_term(learned_nogood.predicates.len().try_into().unwrap());
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
