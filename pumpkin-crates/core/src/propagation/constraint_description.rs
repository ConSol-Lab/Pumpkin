use crate::checkers::Scope;
#[cfg(doc)]
use crate::propagation::ConflictRule;

/// Implemented by the type that holds the data of a constraint, such as the terms and the bound of
/// a linear inequality, independent of the propagator that propagates it.
///
/// The checkers of a constraint are built from its description by the [`ConflictRule`] of the
/// constraint, so they see the same data as the propagator.
pub trait ConstraintDescription {
    /// The variables whose domain changes wake up the retention checker of the constraint.
    fn scope(&self) -> Scope;
}
