use pumpkin_core::asserts::pumpkin_assert_simple;
use pumpkin_core::conflict_resolving::ConflictAnalysisContext;
use pumpkin_core::conflict_resolving::ConflictResolver;
#[cfg(doc)]
use pumpkin_core::predicates::Predicate;
use pumpkin_core::propagation::ReadDomains;

/// [`ConflictResolver`] which does not attempt to learn from conflicts.
///
/// The idea is to backtrack a single level and try the negated [`Predicate`] of the previous
/// decision.
#[derive(Default, Debug, Clone, Copy)]
pub struct NoLearningResolver;

impl ConflictResolver for NoLearningResolver {
    fn resolve_conflict(&mut self, context: &mut ConflictAnalysisContext) {
        let last_decision = context
            .find_last_decision()
            .expect("the solver is not at decision level 0, so there exists a last decision");

        let current_checkpoint = context.get_checkpoint();
        let _ = context.restore_to(current_checkpoint - 1);

        let update_occurred = context
            .post(!last_decision)
            .expect("Expected enqueued predicate to not lead to conflict directly");

        pumpkin_assert_simple!(
            update_occurred,
            "The propagated predicate should not already be true."
        );
    }
}
