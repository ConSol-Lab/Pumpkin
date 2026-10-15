use crate::checkers::CompleteSolution;
use crate::checkers::Scope;
use crate::checkers::ScopeItem;
use crate::propagation::SolutionCheck;

/// Implemented by the type that holds the data that defines a constraint, such as the terms and
/// the bound of a linear inequality, independent of the propagator that propagates it.
///
/// A constraint description holds only what defines the constraint: its variables and constants.
/// Settings that only change how a propagator works, such as the explanation type of the
/// cumulative, are not part of it; the constructor arguments of a propagator hold them next to the
/// constraint description.
///
/// The scope comes from [`ScopeItem`], which is usually derived (`#[derive(ScopeItem)]`) so that
/// every field of the description is part of it.
pub trait ConstraintDescription: ScopeItem {
    /// The variables of the constraint.
    fn scope(&self) -> Scope {
        let mut scope = Scope::default();
        self.add_to_scope(&mut scope);
        scope
    }

    /// Whether `solution` satisfies the constraint.
    fn check_solution(&self, solution: &CompleteSolution<'_>) -> SolutionCheck;
}
