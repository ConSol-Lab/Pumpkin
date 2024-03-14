use crate::basic_types::ConstraintReference;
use crate::basic_types::Literal;
use crate::basic_types::PropositionalConjunction;
#[cfg(doc)]
use crate::engine::propagation::Propagator;
#[cfg(doc)]
use crate::engine::ConstraintSatisfactionSolver;
#[cfg(doc)]
use crate::engine::SATCPMediator;

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
        /// The reference to the conflicting constraint.
        reference: ConstraintReference,
        /// The literal which should be both true and false at the same time (due to the
        /// propagation of a [`Literal`], this could occur due to synchronization in
        /// [`SATCPMediator::synchronise_propositional_trail_based_on_integer_trail`] or due to
        /// clausal propagation).
        literal: Literal,
    },
    /// The conflict is triggered by a [`Propagator`].
    Explanation(PropositionalConjunction),
}
