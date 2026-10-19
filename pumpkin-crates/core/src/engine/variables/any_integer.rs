use enumset::EnumSet;
use itertools::Either;
use pumpkin_checking::CheckerVariable;
use pumpkin_checking::IntExt;
use pumpkin_checking::VariableState;

use super::AffineView;
use super::DomainId;
use super::IntegerVariable;
use super::TransformableVariable;
use crate::engine::Assignments;
use crate::engine::notifications::DomainEvent;
use crate::engine::notifications::OpaqueDomainEvent;
use crate::engine::notifications::Watchers;
use crate::predicates::Predicate;
use crate::predicates::PredicateConstructor;
use crate::propagation::EventDispatcher;
use crate::propagation::EventTarget;
use crate::propagation::LocalId;

/// An [`IntegerVariable`] which is either an integer variable or a [`Predicate`] (interpreted as a
/// 0-1 variable).
///
/// This allows posting constraints, such as linear constraints, over a mix of integer variables
/// and predicates.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AnyInteger {
    DomainId(DomainId),
    Predicate(Predicate),
}

/// Calls `$body` with `$var` bound to the variable inside the [`AnyInteger`].
macro_rules! dispatch {
    ($self:expr, $var:ident => $body:expr) => {
        match $self {
            AnyInteger::DomainId($var) => $body,
            AnyInteger::Predicate($var) => $body,
        }
    };
}

/// Like [`dispatch`], but for bodies returning an iterator.
macro_rules! dispatch_iter {
    ($self:expr, $var:ident => $body:expr) => {
        match $self {
            AnyInteger::DomainId($var) => Either::Left($body),
            AnyInteger::Predicate($var) => Either::Right($body),
        }
    };
}

impl From<DomainId> for AnyInteger {
    fn from(domain_id: DomainId) -> Self {
        AnyInteger::DomainId(domain_id)
    }
}

impl From<Predicate> for AnyInteger {
    fn from(predicate: Predicate) -> Self {
        AnyInteger::Predicate(predicate)
    }
}

impl EventTarget for AnyInteger {
    fn register(
        &self,
        registration: &mut impl EventDispatcher,
        events: EnumSet<DomainEvent>,
        local_id: LocalId,
    ) {
        dispatch!(self, var => var.register(registration, events, local_id))
    }
}

impl CheckerVariable<Predicate> for AnyInteger {
    fn does_atomic_constrain_self(&self, atomic: &Predicate) -> bool {
        dispatch!(self, var => var.does_atomic_constrain_self(atomic))
    }

    fn atomic_less_than(&self, value: i32) -> Predicate {
        dispatch!(self, var => var.atomic_less_than(value))
    }

    fn atomic_greater_than(&self, value: i32) -> Predicate {
        dispatch!(self, var => var.atomic_greater_than(value))
    }

    fn atomic_not_equal(&self, value: i32) -> Predicate {
        dispatch!(self, var => var.atomic_not_equal(value))
    }

    fn atomic_equal(&self, value: i32) -> Predicate {
        dispatch!(self, var => var.atomic_equal(value))
    }

    fn induced_lower_bound(&self, domains: &VariableState<Predicate>) -> IntExt {
        dispatch!(self, var => var.induced_lower_bound(domains))
    }

    fn induced_upper_bound(&self, domains: &VariableState<Predicate>) -> IntExt {
        dispatch!(self, var => var.induced_upper_bound(domains))
    }

    fn induced_fixed_value(&self, domains: &VariableState<Predicate>) -> Option<i32> {
        dispatch!(self, var => var.induced_fixed_value(domains))
    }

    fn induced_domain_contains(&self, domains: &VariableState<Predicate>, value: i32) -> bool {
        dispatch!(self, var => var.induced_domain_contains(domains, value))
    }

    fn induced_holes<'this, 'state>(
        &'this self,
        domains: &'state VariableState<Predicate>,
    ) -> impl Iterator<Item = i32> + 'state
    where
        'this: 'state,
    {
        dispatch_iter!(self, var => var.induced_holes(domains))
    }

    fn iter_induced_domain<'this, 'state>(
        &'this self,
        domains: &'state VariableState<Predicate>,
    ) -> Option<impl Iterator<Item = i32> + 'state>
    where
        'this: 'state,
    {
        match self {
            AnyInteger::DomainId(var) => var.iter_induced_domain(domains).map(Either::Left),
            AnyInteger::Predicate(var) => var.iter_induced_domain(domains).map(Either::Right),
        }
    }
}

impl IntegerVariable for AnyInteger {
    type AffineView = AffineView<Self>;

    fn lower_bound(&self, assignment: &Assignments) -> i32 {
        dispatch!(self, var => var.lower_bound(assignment))
    }

    fn lower_bound_at_trail_position(
        &self,
        assignment: &Assignments,
        trail_position: usize,
    ) -> i32 {
        dispatch!(self, var => var.lower_bound_at_trail_position(assignment, trail_position))
    }

    fn upper_bound(&self, assignment: &Assignments) -> i32 {
        dispatch!(self, var => var.upper_bound(assignment))
    }

    fn upper_bound_at_trail_position(
        &self,
        assignment: &Assignments,
        trail_position: usize,
    ) -> i32 {
        dispatch!(self, var => var.upper_bound_at_trail_position(assignment, trail_position))
    }

    fn contains(&self, assignment: &Assignments, value: i32) -> bool {
        dispatch!(self, var => var.contains(assignment, value))
    }

    fn contains_at_trail_position(
        &self,
        assignment: &Assignments,
        value: i32,
        trail_position: usize,
    ) -> bool {
        dispatch!(self, var => var.contains_at_trail_position(assignment, value, trail_position))
    }

    fn iterate_domain(&self, assignment: &Assignments) -> impl Iterator<Item = i32> {
        dispatch_iter!(self, var => var.iterate_domain(assignment))
    }

    fn unwatch_all(&self, watchers: &mut Watchers<'_>) {
        dispatch!(self, var => var.unwatch_all(watchers))
    }

    fn watch_all_backtrack(&self, watchers: &mut Watchers<'_>, events: EnumSet<DomainEvent>) {
        dispatch!(self, var => var.watch_all_backtrack(watchers, events))
    }

    fn unpack_event(&self, event: OpaqueDomainEvent) -> DomainEvent {
        dispatch!(self, var => var.unpack_event(event))
    }

    fn get_holes_at_current_checkpoint(
        &self,
        assignments: &Assignments,
    ) -> impl Iterator<Item = i32> {
        dispatch_iter!(self, var => var.get_holes_at_current_checkpoint(assignments))
    }

    fn get_holes(&self, assignments: &Assignments) -> impl Iterator<Item = i32> {
        dispatch_iter!(self, var => var.get_holes(assignments))
    }
}

impl PredicateConstructor for AnyInteger {
    type Value = i32;

    fn lower_bound_predicate(&self, bound: Self::Value) -> Predicate {
        dispatch!(self, var => var.lower_bound_predicate(bound))
    }

    fn upper_bound_predicate(&self, bound: Self::Value) -> Predicate {
        dispatch!(self, var => var.upper_bound_predicate(bound))
    }

    fn equality_predicate(&self, bound: Self::Value) -> Predicate {
        dispatch!(self, var => var.equality_predicate(bound))
    }

    fn disequality_predicate(&self, bound: Self::Value) -> Predicate {
        dispatch!(self, var => var.disequality_predicate(bound))
    }
}

impl TransformableVariable<AffineView<AnyInteger>> for AnyInteger {
    fn scaled(&self, scale: i32) -> AffineView<AnyInteger> {
        AffineView::new(*self, scale, 0)
    }

    fn offset(&self, offset: i32) -> AffineView<AnyInteger> {
        AffineView::new(*self, 1, offset)
    }
}
