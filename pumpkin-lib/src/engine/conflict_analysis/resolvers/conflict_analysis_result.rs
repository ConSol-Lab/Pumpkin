use crate::variables::Literal;

#[derive(Clone, Default, Debug)]
/// The outcome of clause learning.
pub struct ConflictAnalysisResult {
    /// The new learned clause with the propagating literal after backjumping at index 0 and the
    /// literal with the next highest decision level at index 1.
    pub learned_literals: Vec<Literal>,
    /// The decision level to backtrack to.
    pub backjump_level: usize,
}
