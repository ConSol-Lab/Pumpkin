use crate::basic_types::SolutionReference;
use crate::branching::SelectionContext;
use crate::engine::predicates::integer_predicate::IntegerPredicate;
use crate::engine::variables::DomainId;
#[cfg(doc)]
use crate::engine::ConstraintSatisfactionSolver;
#[cfg(doc)]
use crate::optimisation::LinearSearch;

/// A trait containing the interface for [`ValueSelector`]s,
/// specifying the appropriate hooks into the solver and the methods required for selecting a value
/// for a given variable.
pub trait ValueSelector<Var> {
    /// Determines which value in the domain of `decision_variable` to branch next on.
    /// The domain of the `decision_variable` variable should have at least 2 values in it (as it
    /// otherwise should not have been selected as `decision_variable`). Returns a
    /// [`IntegerPredicate`] specifying the required change in the domain.
    fn select_value(
        &mut self,
        context: &mut SelectionContext,
        decision_variable: Var,
    ) -> IntegerPredicate;

    /// A function which is called after a [`DomainId`] is unassigned during backtracking (i.e. when
    /// it was fixed but is no longer), specifically, it provides `variable` which is the
    /// [`DomainId`] which has been reset and `value` which is the value to which the variable was
    /// previously fixed. This method could thus be called multiple times in a single
    /// backtracking operation by the solver
    /// (see the `backtrack` method of [`ConstraintSatisfactionSolver`]).
    fn on_unassign_integer(&mut self, _variable: DomainId, _value: i32) {}

    /// This method is called when a solution is found in the optimisation loop of [`LinearSearch`].
    fn on_solution(&mut self, _solution: SolutionReference) {}
}
