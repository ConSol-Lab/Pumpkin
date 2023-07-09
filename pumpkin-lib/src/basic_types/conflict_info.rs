use super::{ClauseReference, Literal, PropositionalConjunction};

pub enum ConflictInfo {
    //virtual binary clauses do not have a constraint reference
    //  these are inlined clauses that are only present in the watch list of the propagation clause propagator
    VirtualBinaryClause {
        lit1: Literal,
        lit2: Literal,
    },
    StandardClause {
        clause_reference: ClauseReference,
    },
    Explanation {
        propositional_conjunction: PropositionalConjunction,
    },
}
