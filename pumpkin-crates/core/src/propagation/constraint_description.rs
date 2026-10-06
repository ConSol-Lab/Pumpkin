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

    /// Whether the values of the variables in `domains` satisfy the constraint.
    ///
    /// Every variable of the constraint has to be fixed; otherwise the result is
    /// [`SolutionCheck::UnfixedVariable`].
    fn check_solution(&self, domains: &dyn DomainView<Predicate>) -> SolutionCheck;
}
