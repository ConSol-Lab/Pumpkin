use super::ConflictAnalysisResult;
use super::ConflictResolver;
use crate::basic_types::ConstraintReference;
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
        let last_decision = context
            .last_decision()
            .expect("Expected a previous decision to exist");

        context.backtrack(context.get_decision_level() - 1);

        if context.get_decision_level() == 0 {
            context.assignments_propositional.increase_decision_level();
            context.assignments_integer.increase_decision_level();
            context.reason_store.increase_decision_level();
        }
        println!(
            "--{:?}",
            context
                .variable_literal_mappings
                .get_predicates(!last_decision)
                .collect::<Vec<_>>()
        );

        let _ = context.enqueue_propagated_literal(!last_decision, ConstraintReference::NULL);
        Ok(())
    }
}
