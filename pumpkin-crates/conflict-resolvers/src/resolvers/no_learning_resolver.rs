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

        context.backtrack(context.get_checkpoint() - 1);
        context.post_predicate_without_reason(!last_decision);
    }
}
