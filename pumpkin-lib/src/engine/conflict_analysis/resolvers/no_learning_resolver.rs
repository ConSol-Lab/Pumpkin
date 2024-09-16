use super::ConflictResolver;
use crate::engine::conflict_analysis::ConflictAnalysisNogoodContext;
use crate::engine::conflict_analysis::LearnedNogood;

#[derive(Default, Debug, Clone, Copy)]
pub struct NoLearningResolver;

impl ConflictResolver for NoLearningResolver {
    fn resolve_conflict(
        &mut self,
        _context: &mut ConflictAnalysisNogoodContext,
    ) -> Option<LearnedNogood> {
        todo!()
    }

    fn process(
        &mut self,
        _context: &mut ConflictAnalysisNogoodContext,
        _learned_nogood: &Option<LearnedNogood>,
    ) -> Result<(), ()> {
        todo!()
    }
}
