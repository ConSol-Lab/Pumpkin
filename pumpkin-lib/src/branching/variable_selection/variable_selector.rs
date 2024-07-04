use crate::branching::SelectionContext;
use crate::engine::variables::DomainId;
use crate::engine::variables::Literal;
use crate::engine::variables::PropositionalVariable;
#[cfg(doc)]
use crate::engine::ConstraintSatisfactionSolver;
#[cfg(doc)]
use crate::optimisation::LinearSearch;

/// A trait containing the interface for [`VariableSelector`]s,
/// specifying the appropriate hooks into the solver and the methods required for selecting
/// variables.
pub trait VariableSelector<Var> {
    /// Determines which variable to select next if there are any left to branch on.
    /// Should only return [`None`] when all variables which have been passed to the
    /// [`VariableSelector`] have been assigned. Otherwise it should return the variable to
    /// branch on next.
    fn select_variable(&mut self, context: &SelectionContext) -> Option<Var>;

    /// A function which is called after a conflict has been found and processed but (currently)
    /// does not provide any additional information.
    fn on_conflict(&mut self) {}

    /// A function which is called after a [`Literal`] is unassigned during backtracking (i.e. when
    /// it was fixed but is no longer), specifically, it provides `literal` which is the
    /// [Literal] which has been reset. This method could thus be called multiple times in a
    /// single backtracking operation by the solver
    /// (see the `backtrack` method of [`ConstraintSatisfactionSolver`]).
    fn on_unassign_literal(&mut self, _literal: Literal) {}

    /// A function which is called after a [`DomainId`] is unassigned during backtracking (i.e. when
    /// it was fixed but is no longer), specifically, it provides `variable` which is the
    /// [`DomainId`] which has been reset. This method could thus be called multiple times in a
    /// single backtracking operation by the solver
    /// (see the `backtrack` method of [`ConstraintSatisfactionSolver`]).
    fn on_unassign_integer(&mut self, _variable: DomainId, _value: i32) {}

    /// A function which is called when a [`Literal`] appears in a conflict during conflict analysis
    /// (see the `compute_1uip` method of [`ConstraintSatisfactionSolver`]).
    fn on_appearance_in_conflict_literal(&mut self, _literal: Literal) {}

    /// A function which is called when a variable appears in a conflict during conflict analysis
    /// (see the `compute_1uip` method of [`ConstraintSatisfactionSolver`]).
    fn on_appearance_in_conflict_integer(&mut self, _variable: DomainId) {}

    /// A function which is called when new [`PropositionalVariable`]s are added to the solver when
    /// encoding the objective function this method is currently only called during
    /// [`LinearSearch`] when the encoding of the objective function is added.
    ///
    /// Note that this method provides **all** [`PropositionalVariable`]s and it is up to the
    /// [`VariableSelector`] to determine how to handle it.
    fn on_encoding_objective_function(&mut self, _all_variables: &[PropositionalVariable]) {}
}
