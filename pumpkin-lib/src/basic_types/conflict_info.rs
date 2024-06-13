use thiserror::Error;

use crate::basic_types::ConstraintReference;
use crate::basic_types::PropositionalConjunction;
#[cfg(doc)]
use crate::engine::propagation::Propagator;
use crate::engine::propagation::PropagatorId;
use crate::engine::variables::Literal;
#[cfg(doc)]
use crate::engine::ConstraintSatisfactionSolver;
#[cfg(doc)]
use crate::engine::VariableLiteralMappings;

#[derive(Debug, PartialEq, Eq, Clone)]
// Allow the larger `Explanation` variant since this `ConflictInfo` type is not used very often,
//  nor in large amounts.
#[allow(variant_size_differences)]
/// Describes a conflict in [`ConstraintSatisfactionSolver`].
pub enum ConflictInfo {
    // virtual binary clauses do not have a constraint reference
    //  these are inlined clauses that are only present in the watch list of the propagation
    // clause propagator
    VirtualBinaryClause {
        lit1: Literal,
        lit2: Literal,
    },
    /// The conflict is triggered in the propositional representation.
    Propagation {
        /// The reference to the constraint propagating `literal` to the opposite polarity of the
        /// current assignment.
        reference: ConstraintReference,
        /// The literal which should be both true and false at the same time.
        literal: Literal,
    },
    /// The conflict is triggered by a [`Propagator`].
    Explanation(PropositionalConjunction),
}

impl ConflictInfo {
    pub(crate) fn into_stored(self, propagator: PropagatorId) -> StoredConflictInfo {
        match self {
            ConflictInfo::VirtualBinaryClause { lit1, lit2 } => {
                StoredConflictInfo::VirtualBinaryClause { lit1, lit2 }
            }
            ConflictInfo::Propagation { reference, literal } => {
                StoredConflictInfo::Propagation { reference, literal }
            }
            ConflictInfo::Explanation(conjunction) => StoredConflictInfo::Explanation {
                conjunction,
                propagator,
            },
        }
    }
}

/// A conflict info which can be stored in the solver. It exists to annotate the
/// [`ConflictInfo::Explanation`] variant with the propagator which caused the conflict.
#[derive(Debug, PartialEq, Eq, Clone)]
#[allow(variant_size_differences)]
pub enum StoredConflictInfo {
    // virtual binary clauses do not have a constraint reference
    //  these are inlined clauses that are only present in the watch list of the propagation
    // clause propagator
    VirtualBinaryClause {
        lit1: Literal,
        lit2: Literal,
    },
    /// The conflict is triggered in the propositional representation.
    Propagation {
        /// The reference to the constraint propagating `literal` to the opposite polarity of the
        /// current assignment.
        reference: ConstraintReference,
        /// The literal which should be both true and false at the same time.
        literal: Literal,
    },
    /// The conflict is triggered by a [`Propagator`].
    Explanation {
        conjunction: PropositionalConjunction,
        propagator: PropagatorId,
    },
}

#[derive(Clone, Copy, Debug, Error)]
#[error("missing the propagator that caused the conflict")]
pub struct MissingPropagator;

impl TryFrom<ConflictInfo> for StoredConflictInfo {
    type Error = MissingPropagator;

    fn try_from(value: ConflictInfo) -> Result<Self, Self::Error> {
        match value {
            ConflictInfo::VirtualBinaryClause { lit1, lit2 } => {
                Ok(StoredConflictInfo::VirtualBinaryClause { lit1, lit2 })
            }
            ConflictInfo::Propagation { reference, literal } => {
                Ok(StoredConflictInfo::Propagation { reference, literal })
            }
            ConflictInfo::Explanation(_) => Err(MissingPropagator),
        }
    }
}
