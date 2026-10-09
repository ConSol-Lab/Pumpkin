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
    /// Returns [`SolutionCheck::ConstraintSatisfied`] when the constraint holds whatever values the
    /// variables that are not fixed take, [`SolutionCheck::ConstraintViolated`] when it fails
    /// whatever they take, and [`SolutionCheck::Unknown`] when the outcome depends on them.
    ///
    /// Note that not all variables need to be assigned. A variable that is not fixed does not make
    /// the outcome [`SolutionCheck::Unknown`] when the constraint is decided without it,
    /// such as a nogood with a false predicate.
    fn check_solution(&self, domains: &dyn DomainView<Predicate>) -> SolutionCheck;
}
