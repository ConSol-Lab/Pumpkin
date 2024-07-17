use crate::basic_types::SolutionReference;
use crate::branching::SelectionContext;
use crate::engine::predicates::predicate::Predicate;
use crate::engine::variables::DomainId;
use crate::engine::variables::Literal;

/// A trait containing the interface for [`ValueSelector`]s,
/// specifying the appropriate hooks into the solver and the methods required for selecting a value
/// for a given variable.
pub trait ValueSelector<Var> {
    /// Determines which value in the domain of `decision_variable` to branch next on.
    /// The domain of the `decision_variable` variable should have at least 2 values in it (as it
    /// otherwise should not have been selected as `decision_variable`). Returns a [`Predicate`]
    /// specifying the required change in the domain.
    fn select_value(&mut self, context: &mut SelectionContext, decision_variable: Var)
        -> Predicate;

    /// A function which is called after a [`Literal`] is unassigned during backtracking (i.e. when
    /// it was fixed but is no longer), specifically, it provides `literal` which is the
    /// [`Literal`] which has been reset. This method could thus be called multiple times in a
    /// single backtracking operation by the solver.
    fn on_unassign_literal(&mut self, _literal: Literal) {}

    /// A function which is called after a [`DomainId`] is unassigned during backtracking (i.e. when
    /// it was fixed but is no longer), specifically, it provides `variable` which is the
    /// [`DomainId`] which has been reset and `value` which is the value to which the variable was
    /// previously fixed. This method could thus be called multiple times in a single
    /// backtracking operation by the solver.
    fn on_unassign_integer(&mut self, _variable: DomainId, _value: i32) {}

    /// This method is called when a solution is found; either when iterating over all solutions in
    /// the case of a satisfiable problem or on solutions of increasing quality when solving an
    /// optimisation problem.
    fn on_solution(&mut self, _solution: SolutionReference) {}
}
