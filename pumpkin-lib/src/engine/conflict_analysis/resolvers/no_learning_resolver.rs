use super::ConflictAnalysisResult;
use super::ConflictResolver;
use super::ResolutionConflictAnalyser;
use crate::basic_types::moving_averages::moving_average::MovingAverage;
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
            let analysis_result = self.resolution.resolve_conflict(context).unwrap();

            let lbd = context.learned_clause_manager.compute_lbd_for_literals(
                &analysis_result.learned_literals,
                context.assignments_propositional,
            );

            context
                .counters
                .average_learned_clause_length
                .add_term(analysis_result.learned_literals.len().try_into().unwrap());
            context.counters.average_lbd.add_term(lbd.into());
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
