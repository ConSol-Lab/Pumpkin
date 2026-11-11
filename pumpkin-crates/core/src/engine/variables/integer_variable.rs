use std::fmt::Debug;

use enumset::EnumSet;
use pumpkin_checking::CheckerVariable;

use super::TransformableVariable;
use crate::engine::Assignments;
use crate::engine::notifications::DomainEvent;
use crate::engine::notifications::OpaqueDomainEvent;
use crate::engine::notifications::Watchers;
use crate::engine::predicates::predicate_constructor::PredicateConstructor;
use crate::predicates::Predicate;
use crate::propagation::EventTarget;

/// A trait specifying the required behaviour of an integer variable such as retrieving a
/// lower-bound ([`IntegerVariable::lower_bound`]).
pub trait IntegerVariable:
    Clone
    + PredicateConstructor<Value = i32>
    + TransformableVariable<Self::AffineView>
    + Debug
    + CheckerVariable<Predicate>
    + EventTarget
{
    type AffineView: IntegerVariable;

    /// Get the lower bound of the variable.
    fn lower_bound(&self, assignment: &Assignments) -> i32;

    /// Get the lower bound of the variable at the given trail position.
    fn lower_bound_at_trail_position(&self, assignment: &Assignments, trail_position: usize)
    -> i32;

    /// Get the upper bound of the variable.
    fn upper_bound(&self, assignment: &Assignments) -> i32;

    /// Get the upper bound of the variable at the given trail position.
    fn upper_bound_at_trail_position(&self, assignment: &Assignments, trail_position: usize)
    -> i32;

    /// Determine whether the value is in the domain of this variable.
    fn contains(&self, assignment: &Assignments, value: i32) -> bool;

    /// Determine whether the value is in the domain of this variable at the given trail position.
    fn contains_at_trail_position(
        &self,
        assignment: &Assignments,
        value: i32,
        trail_position: usize,
    ) -> bool;

    /// Iterate over the values of the domain.
    fn iterate_domain(&self, assignment: &Assignments) -> impl Iterator<Item = i32>;

    /// Remove the watcher on this variable.
    fn unwatch_all(&self, watchers: &mut Watchers<'_>);

    fn watch_all_backtrack(&self, watchers: &mut Watchers<'_>, events: EnumSet<DomainEvent>);

    /// Decode a domain event for this variable.
    fn unpack_event(&self, event: OpaqueDomainEvent) -> DomainEvent;

    /// Returns all of the holes in the domain which were created at the current decision level.
    ///
    /// Values which the variable can never take, regardless of the assignment, are not created at
    /// any decision level and are thus not returned; e.g., the values which are not a multiple of 2
    /// for the view `2x`. These are only returned by [`IntegerVariable::get_holes`].
    fn get_holes_at_current_checkpoint(
        &self,
        assignments: &Assignments,
    ) -> impl Iterator<Item = i32>;

    /// Returns all of the holes in the domain; i.e., every value strictly between the bounds which
    /// is not in the domain. It can additionally contain removed values which are outside of the
    /// current bounds.
    fn get_holes(&self, assignments: &Assignments) -> impl Iterator<Item = i32>;
}
