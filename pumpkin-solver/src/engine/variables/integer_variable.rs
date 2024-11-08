use enumset::EnumSet;

use super::TransformableVariable;
use crate::engine::opaque_domain_event::OpaqueDomainEvent;
use crate::engine::predicates::predicate_constructor::PredicateConstructor;
use crate::engine::reason::ReasonRef;
use crate::engine::Assignments;
use crate::engine::EmptyDomain;
use crate::engine::IntDomainEvent;
use crate::engine::Watchers;

/// A trait specifying the required behaviour of an integer variable such as retrieving a
/// lower-bound ([`IntegerVariable::lower_bound`]) or adjusting the bounds
/// ([`IntegerVariable::set_lower_bound`]).
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

    /// Remove a value from the domain of this variable.
    fn remove(
        &self,
        assignment: &mut Assignments,
        value: i32,
        reason: Option<ReasonRef>,
    ) -> Result<(), EmptyDomain>;

    /// Tighten the lower bound of the domain of this variable.
    fn set_lower_bound(
        &self,
        assignment: &mut Assignments,
        value: i32,
        reason: Option<ReasonRef>,
    ) -> Result<(), EmptyDomain>;

    /// Tighten the upper bound of the domain of this variable.
    fn set_upper_bound(
        &self,
        assignment: &mut Assignments,
        value: i32,
        reason: Option<ReasonRef>,
    ) -> Result<(), EmptyDomain>;

    /// Register a watch for this variable on the given domain events.
    fn watch_all(&self, watchers: &mut Watchers<'_>, events: EnumSet<IntDomainEvent>);

    fn watch_all_backtrack(&self, watchers: &mut Watchers<'_>, events: EnumSet<IntDomainEvent>);

    /// Decode a domain event for this variable.
    fn unpack_event(&self, event: OpaqueDomainEvent) -> IntDomainEvent;
}
