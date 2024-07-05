use crate::engine::predicates::predicate::Predicate;

#[derive(Clone, Copy, Debug)]
pub struct ConflictAnalysisNogoodContext {}

impl ConflictAnalysisNogoodContext {
    pub fn get_conflict_nogood(&self) -> Vec<Predicate> {
        todo!();
    }

    pub fn get_propagation_reason(&self, _predicate: Predicate) -> Vec<Predicate> {
        todo!();
    }
}
