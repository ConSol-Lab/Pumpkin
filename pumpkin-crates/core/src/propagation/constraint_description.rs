use pumpkin_checking::DomainView;

use crate::checkers::Scope;
use crate::predicates::Predicate;
#[cfg(doc)]
use crate::propagation::ConflictRule;
use crate::propagation::SolutionCheck;

/// Implemented by the type that holds the data of a constraint, such as the terms and the bound of
/// a linear inequality, independent of the propagator that propagates it.
///
/// The checkers of a constraint are built from its description by the [`ConflictRule`] of the
/// constraint, so they see the same data as the propagator.
pub trait ConstraintDescription {
    /// The variables whose domain changes wake up the retention checker of the constraint.
    fn scope(&self) -> Scope;

    /// Whether `domains` satisfy the constraint.
    ///
    /// A variable that is not fixed does not make the outcome [`SolutionCheck::Unknown`] when the
    /// constraint is decided without it, such as a nogood with a false predicate. A constraint may
    /// also answer [`SolutionCheck::Unknown`] when it does not reason about partial assignments.
    fn check_solution(&self, domains: &dyn DomainView<Predicate>) -> SolutionCheck;
}
