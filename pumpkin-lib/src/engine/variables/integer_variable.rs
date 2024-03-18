use enumset::EnumSet;

use crate::basic_types::Predicate;
use crate::basic_types::PredicateConstructor;
use crate::engine::opaque_domain_event::OpaqueDomainEvent;
use crate::engine::reason::ReasonRef;
use crate::engine::AssignmentsInteger;
use crate::engine::EmptyDomain;
use crate::engine::IntDomainEvent;
use crate::engine::Watchers;

pub trait IntegerVariable: Clone + PredicateConstructor<Value = i32> {
    type AffineView: IntegerVariable;

    /// Get the lower bound of the variable.
    fn lower_bound(&self, assignment: &AssignmentsInteger) -> i32;

    /// Get the upper bound of the variable.
    fn upper_bound(&self, assignment: &AssignmentsInteger) -> i32;

    /// Determine whether the value is in the domain of this variable.
    fn contains(&self, assignment: &AssignmentsInteger, value: i32) -> bool;

    /// Get a predicate description (bounds + holes) of the domain of this variable.
    /// N.B. can be very expensive with large domains, and very large with holey domains
    ///
    /// This should not be used to explicitly check for holes in the domain, but only to build
    /// explanations. If views change the observed domain, they will not change this description,
    /// because it should be a description of the domain in the solver.
    fn describe_domain(&self, assignment: &AssignmentsInteger) -> Vec<Predicate>;

    /// Remove a value from the domain of this variable.
    fn remove(
        &self,
        assignment: &mut AssignmentsInteger,
        value: i32,
        reason: Option<ReasonRef>,
    ) -> Result<(), EmptyDomain>;

    /// Tighten the lower bound of the domain of this variable.
    fn set_lower_bound(
        &self,
        assignment: &mut AssignmentsInteger,
        value: i32,
        reason: Option<ReasonRef>,
    ) -> Result<(), EmptyDomain>;

    /// Tighten the upper bound of the domain of this variable.
    fn set_upper_bound(
        &self,
        assignment: &mut AssignmentsInteger,
        value: i32,
        reason: Option<ReasonRef>,
    ) -> Result<(), EmptyDomain>;

    /// Register a watch for this variable on the given domain events.
    fn watch_all(&self, watchers: &mut Watchers<'_>, events: EnumSet<IntDomainEvent>);

    /// Decode a domain event for this variable.
    fn unpack_event(&self, event: OpaqueDomainEvent) -> IntDomainEvent;

    /// Get a variable which domain is scaled compared to the domain of self.
    ///
    /// The scaled domain will have holes in it. E.g. if we have `dom(x) = {1, 2}`, then
    /// `dom(x.scaled(2)) = {2, 4}` and *not* `dom(x.scaled(2)) = {1, 2, 3, 4}`.
    fn scaled(&self, scale: i32) -> Self::AffineView;

    /// Get a variable which domain has a constant offset to the domain of self.
    fn offset(&self, offset: i32) -> Self::AffineView;
}
