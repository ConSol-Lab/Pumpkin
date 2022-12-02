use super::Literal;

pub enum BranchingDecision {
    Assumption { assumption_literal: Literal },
    StandardDecision { decision_literal: Literal },
}
