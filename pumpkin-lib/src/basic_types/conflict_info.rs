use crate::basic_types::ConstraintReference;
use crate::basic_types::PropositionalConjunction;
#[cfg(doc)]
use crate::engine::propagation::Propagator;
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
