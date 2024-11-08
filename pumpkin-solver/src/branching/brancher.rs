#[cfg(doc)]
use crate::basic_types::Random;
use crate::basic_types::SolutionReference;
#[cfg(doc)]
use crate::branching;
#[cfg(doc)]
use crate::branching::value_selection::ValueSelector;
#[cfg(doc)]
use crate::branching::variable_selection::VariableSelector;
use crate::branching::SelectionContext;
use crate::engine::predicates::predicate::Predicate;
use crate::engine::variables::DomainId;
use crate::engine::Assignments;
#[cfg(doc)]
use crate::results::solution_iterator::SolutionIterator;
#[cfg(doc)]
use crate::Solver;

/// A trait for definining a branching strategy (oftentimes utilising a [`VariableSelector`] and a
/// [`ValueSelector`]).
///
/// In general, implementations of this trait define how the search of the solver proceeds (i.e. it
/// controls how the solver determines which part of the search space to explore). It is required
/// that the resulting decision creates a smaller domain for at least 1 of the variables (and more
/// domains can be affected due to subsequent inference). See [`branching`] for
/// example usages.
///
/// If the [`Brancher`] (or any component thereof) is implemented incorrectly then the
/// behaviour of the solver is undefined.
pub trait Brancher {
    /// Returns the next decision concerning a single variable and value; it returns the
    /// [`Predicate`] corresponding to this decision (or [`None`] if all variables under
    /// consideration are assigned).
    ///
    /// Note that this method **cannot** perform the assignment of the decision, it should only
    /// return a suggestion in the form of a [`Predicate`]; the [`SelectionContext`] is
    /// only mutable to account for the usage of random generators (e.g. see [`Random`]).
    fn next_decision(&mut self, context: &mut SelectionContext) -> Option<Predicate>;

    /// A function which is called after a conflict has been found and processed but (currently)
    /// does not provide any additional information.
    fn on_conflict(&mut self) {}

    fn on_backtrack(&mut self) {}

    /// This method is called when a solution is found by the [`Solver`].
    fn on_solution(&mut self, _solution: SolutionReference) {}

    /// A function which is called after a [`DomainId`] is unassigned during backtracking (i.e. when
    /// it was fixed but is no longer), specifically, it provides `variable` which is the
    /// [`DomainId`] which has been reset. This method could thus be called multiple times in a
    /// single backtracking operation by the solver.
    fn on_unassign_integer(&mut self, _variable: DomainId, _value: i32) {}

    /// A function which is called when a [`Predicate`] appears in a conflict during conflict
    /// analysis.
    fn on_appearance_in_conflict_predicate(&mut self, _predicate: Predicate) {}

    /// This method is called whenever a restart is performed.
    fn on_restart(&mut self) {}

    /// Called after backtracking.
    /// Used to reset internal data structures to account for the backtrack.
    fn synchronise(&mut self, _assignments: &Assignments) {}

    /// This method returns whether a restart is *currently* pointless for the [`Brancher`].
    ///
    /// For example, if a [`Brancher`] is using a static search strategy then a restart is
    /// pointless; however, if a [`Brancher`] is using a variable selector which
    /// changes throughout the search process then restarting is not pointless.
    ///
    /// Note that even if the [`Brancher`] has indicated that a restart is pointless, it could be
    /// that the restart is still performed (e.g. if this [`Brancher`] is a subcomponent of another
    /// [`Brancher`] and it is not the only `is_restart_pointless` response which is taken into
    /// account).
    fn is_restart_pointless(&mut self) -> bool {
        true
    }
}
