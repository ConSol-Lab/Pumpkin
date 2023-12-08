use super::{ConstraintReference, Literal, PropositionalConjunction};

#[derive(Debug, PartialEq, Eq)]
pub enum ConflictInfo {
    //virtual binary clauses do not have a constraint reference
    //  these are inlined clauses that are only present in the watch list of the propagation clause propagator
    VirtualBinaryClause {
        lit1: Literal,
        lit2: Literal,
    },
    Propagation {
        reference: ConstraintReference,
        literal: Literal,
    },
    Explanation(PropositionalConjunction),
}
