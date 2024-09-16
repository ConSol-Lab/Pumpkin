use super::ConflictAnalysisNogoodContext;
use super::LearnedNogood;

pub(crate) trait ConflictResolver {
    fn resolve_conflict(
        &mut self,
        context: &mut ConflictAnalysisNogoodContext,
    ) -> Option<LearnedNogood>;

    fn process(
        &mut self,
        context: &mut ConflictAnalysisNogoodContext,
        learned_nogood: &Option<LearnedNogood>,
    ) -> Result<(), ()>;
}
