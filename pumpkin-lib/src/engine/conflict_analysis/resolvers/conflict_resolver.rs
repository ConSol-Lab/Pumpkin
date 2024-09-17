use super::ConflictAnalysisResult;
use crate::engine::conflict_analysis::ConflictAnalysisContext;

pub trait ConflictResolver {
    fn resolve_conflict(
        &mut self,
        context: &mut ConflictAnalysisContext,
    ) -> Option<ConflictAnalysisResult>;

    #[allow(clippy::result_unit_err)]
    fn process(
        &mut self,
        context: &mut ConflictAnalysisContext,
        learned_nogood: &Option<ConflictAnalysisResult>,
    ) -> Result<(), ()>;
}
