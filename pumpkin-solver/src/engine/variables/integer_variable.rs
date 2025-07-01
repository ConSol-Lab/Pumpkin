use enumset::EnumSet;

use super::TransformableVariable;
use crate::engine::notifications::DomainEvent;
use crate::engine::notifications::OpaqueDomainEvent;
use crate::engine::notifications::Watchers;
use crate::engine::predicates::predicate_constructor::PredicateConstructor;
use crate::engine::Assignments;

/// A trait specifying the required behaviour of an integer variable such as retrieving a
/// lower-bound ([`IntegerVariable::lower_bound`]).
pub trait IntegerVariable:
    Clone + PredicateConstructor<Value = i32> + TransformableVariable<Self::AffineView>
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

    /// Register a watch for this variable on the given domain events.
    fn watch_all(&self, watchers: &mut Watchers<'_>, events: EnumSet<DomainEvent>);

    fn watch_all_backtrack(&self, watchers: &mut Watchers<'_>, events: EnumSet<DomainEvent>);

    /// Decode a domain event for this variable.
    fn unpack_event(&self, event: OpaqueDomainEvent) -> DomainEvent;

    /// Returns all of the holes in the domain which were created at the current decision level
    fn get_holes_on_current_decision_level(
        &self,
        assignments: &Assignments,
    ) -> impl Iterator<Item = i32>;

    /// Returns all of the holes in the domain
    fn get_holes(&self, assignments: &Assignments) -> impl Iterator<Item = i32>;
}
