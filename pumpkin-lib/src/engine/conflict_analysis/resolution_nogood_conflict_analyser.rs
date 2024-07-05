use super::ConflictAnalysisNogoodContext;
use crate::engine::predicates::predicate::Predicate;

#[derive(Clone, Copy, Debug, Default)]
pub struct ResolutionNogoodConflictAnalyser {}

#[derive(Clone, Debug)]
pub struct LearnedNogood {
    pub predicates: Vec<Predicate>,
    pub backjump_level: usize,
}

impl ResolutionNogoodConflictAnalyser {
    // Computes the nogood according to the 1UIP scheme.
    // The asserting nogood is at position [0], and the second decision level positioned predicate
    // is at position [1].
    pub fn compute_1uip(&mut self, _context: &mut ConflictAnalysisNogoodContext) -> LearnedNogood {
        todo!();
    }
}
