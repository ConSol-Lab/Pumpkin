use pumpkin_checking::DomainView;

use crate::checkers::Scope;
use crate::checkers::ScopeItem;
use crate::predicates::Predicate;
use crate::propagation::SolutionCheck;

/// A constraint description holds the data that defines a constraint: its variables and constants.
/// Propagators and its checkers are built based on constraint descriptions.
pub trait ConstraintDescription: ScopeItem {
    /// The variables of the constraint.
    fn scope(&self) -> Scope {
        let mut scope = Scope::default();
        self.add_to_scope(&mut scope);
        scope
    }

    /// Used to check whether the solution reported by Pumpkin is feasible.
    ///
    /// Returns [`SolutionCheck::ConstraintSatisfied`] when the constraint holds,
    /// [`SolutionCheck::ConstraintViolated`] when the constraint is violated,
    /// and [`SolutionCheck::Unknown`] when there is a non-unit domain,
    /// where feasibility cannot be determined without search.
    ///
    /// Note that this implies that not all variables need to be assigned.
    /// A variable that is not fixed does not make the outcome [`SolutionCheck::Unknown`]
    /// when the constraint is decided without it, such as a nogood with a false predicate.
    ///
    /// However this functionality will be change later, when we will pass a solution
    /// that has all domains as singletons.
    fn check_solution(&self, domains: &dyn DomainView<Predicate>) -> SolutionCheck;
}
