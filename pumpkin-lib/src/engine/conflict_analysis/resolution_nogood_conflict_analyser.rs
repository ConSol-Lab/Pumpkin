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
        // get conflict nogood
        // while not propagating
        //      take the predicate last assigned on the trail
        //      get its reason
        //          for now no lazy, so just ask for the reason based on the trail entry
        //      bump the LBD and activity of the reason
        //      replace it with its reason
        //      do semantic minimisation -> not strictly necessary?
        todo!();
    }
}
