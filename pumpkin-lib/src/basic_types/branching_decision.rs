use crate::engine::variables::Literal;

/// A SAT solver branching decision.
///
/// This distinguishes between the initial solver phase where assumptions are posted, and the
///  search phase where standard decisions are made.
#[derive(Debug, Clone, Copy)]
pub enum BranchingDecision {
    /// An assumption, given as part of the initialisation of the solver.
    Assumption { assumption_literal: Literal },
    /// A usual SAT solver decision during search.
    StandardDecision { decision_literal: Literal },
}
