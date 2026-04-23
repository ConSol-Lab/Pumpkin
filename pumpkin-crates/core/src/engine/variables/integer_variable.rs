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
use crate::variables::AffineView;
use crate::variables::DomainId;

/// A trait specifying the required behaviour of an integer variable such as retrieving a
/// lower-bound ([`IntegerVariable::lower_bound`]).
pub trait IntegerVariable:
    Clone
    + PredicateConstructor<Value = i32>
    + TransformableVariable<Self::AffineView>
    + Debug
    + CheckerVariable<Predicate>
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

    /// Remove the watcher on this variable.
    fn unwatch_all(&self, watchers: &mut Watchers<'_>);

    fn watch_all_backtrack(&self, watchers: &mut Watchers<'_>, events: EnumSet<DomainEvent>);

    /// Decode a domain event for this variable.
    fn unpack_event(&self, event: OpaqueDomainEvent) -> DomainEvent;

    /// Returns all of the holes in the domain which were created at the current decision level
    fn get_holes_at_current_checkpoint(
        &self,
        assignments: &Assignments,
    ) -> impl Iterator<Item = i32>;

    /// Returns all of the holes in the domain
    fn get_holes(&self, assignments: &Assignments) -> impl Iterator<Item = i32>;
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum IntegerVariableEnum {
    DomainId(AffineView<DomainId>),
    Predicate(AffineView<Predicate>),
}

impl From<AffineView<DomainId>> for IntegerVariableEnum {
    fn from(value: AffineView<DomainId>) -> Self {
        IntegerVariableEnum::DomainId(value)
    }
}

impl From<DomainId> for IntegerVariableEnum {
    fn from(value: DomainId) -> Self {
        IntegerVariableEnum::DomainId(value.scaled(1))
    }
}

impl From<AffineView<Predicate>> for IntegerVariableEnum {
    fn from(value: AffineView<Predicate>) -> Self {
        IntegerVariableEnum::Predicate(value)
    }
}

impl From<Predicate> for IntegerVariableEnum {
    fn from(value: Predicate) -> Self {
        IntegerVariableEnum::Predicate(value.scaled(1))
    }
}

impl PredicateConstructor for IntegerVariableEnum {
    type Value = i32;

    fn lower_bound_predicate(&self, bound: Self::Value) -> Predicate {
        match self {
            IntegerVariableEnum::DomainId(domain_id) => domain_id.lower_bound_predicate(bound),
            IntegerVariableEnum::Predicate(literal) => literal.lower_bound_predicate(bound),
        }
    }

    fn upper_bound_predicate(&self, bound: Self::Value) -> Predicate {
        match self {
            IntegerVariableEnum::DomainId(domain_id) => domain_id.upper_bound_predicate(bound),
            IntegerVariableEnum::Predicate(literal) => literal.upper_bound_predicate(bound),
        }
    }

    fn equality_predicate(&self, bound: Self::Value) -> Predicate {
        match self {
            IntegerVariableEnum::DomainId(domain_id) => domain_id.equality_predicate(bound),
            IntegerVariableEnum::Predicate(literal) => literal.equality_predicate(bound),
        }
    }

    fn disequality_predicate(&self, bound: Self::Value) -> Predicate {
        match self {
            IntegerVariableEnum::DomainId(domain_id) => domain_id.disequality_predicate(bound),
            IntegerVariableEnum::Predicate(literal) => literal.disequality_predicate(bound),
        }
    }
}

impl TransformableVariable<IntegerVariableEnum> for IntegerVariableEnum {
    fn scaled(&self, scale: i32) -> IntegerVariableEnum {
        match self {
            IntegerVariableEnum::DomainId(domain_id) => domain_id.scaled(scale).into(),
            IntegerVariableEnum::Predicate(literal) => literal.scaled(scale).into(),
        }
    }

    fn offset(&self, offset: i32) -> IntegerVariableEnum {
        match self {
            IntegerVariableEnum::DomainId(domain_id) => domain_id.offset(offset).into(),
            IntegerVariableEnum::Predicate(literal) => literal.offset(offset).into(),
        }
    }
}

impl IntegerVariable for IntegerVariableEnum {
    type AffineView = IntegerVariableEnum;

    fn lower_bound(&self, assignment: &Assignments) -> i32 {
        match self {
            IntegerVariableEnum::DomainId(domain_id) => domain_id.lower_bound(assignment),
            IntegerVariableEnum::Predicate(literal) => literal.lower_bound(assignment),
        }
    }

    fn lower_bound_at_trail_position(
        &self,
        assignment: &Assignments,
        trail_position: usize,
    ) -> i32 {
        match self {
            IntegerVariableEnum::DomainId(domain_id) => {
                domain_id.lower_bound_at_trail_position(assignment, trail_position)
            }
            IntegerVariableEnum::Predicate(literal) => {
                literal.lower_bound_at_trail_position(assignment, trail_position)
            }
        }
    }

    fn upper_bound(&self, assignment: &Assignments) -> i32 {
        match self {
            IntegerVariableEnum::DomainId(domain_id) => domain_id.upper_bound(assignment),
            IntegerVariableEnum::Predicate(literal) => literal.upper_bound(assignment),
        }
    }

    fn upper_bound_at_trail_position(
        &self,
        assignment: &Assignments,
        trail_position: usize,
    ) -> i32 {
        match self {
            IntegerVariableEnum::DomainId(domain_id) => {
                domain_id.upper_bound_at_trail_position(assignment, trail_position)
            }
            IntegerVariableEnum::Predicate(literal) => {
                literal.upper_bound_at_trail_position(assignment, trail_position)
            }
        }
    }

    fn contains(&self, assignment: &Assignments, value: i32) -> bool {
        match self {
            IntegerVariableEnum::DomainId(domain_id) => domain_id.contains(assignment, value),
            IntegerVariableEnum::Predicate(literal) => literal.contains(assignment, value),
        }
    }

    fn contains_at_trail_position(
        &self,
        assignment: &Assignments,
        value: i32,
        trail_position: usize,
    ) -> bool {
        match self {
            IntegerVariableEnum::DomainId(domain_id) => {
                domain_id.contains_at_trail_position(assignment, value, trail_position)
            }
            IntegerVariableEnum::Predicate(literal) => {
                literal.contains_at_trail_position(assignment, value, trail_position)
            }
        }
    }

    fn iterate_domain(&self, assignment: &Assignments) -> impl Iterator<Item = i32> {
        match self {
            IntegerVariableEnum::DomainId(domain_id) => domain_id
                .iterate_domain(assignment)
                .collect::<Vec<_>>()
                .into_iter(),
            IntegerVariableEnum::Predicate(literal) => literal
                .iterate_domain(assignment)
                .collect::<Vec<_>>()
                .into_iter(),
        }
    }

    fn watch_all(&self, watchers: &mut Watchers<'_>, events: EnumSet<DomainEvent>) {
        match self {
            IntegerVariableEnum::DomainId(domain_id) => domain_id.watch_all(watchers, events),
            IntegerVariableEnum::Predicate(literal) => literal.watch_all(watchers, events),
        }
    }

    fn unwatch_all(&self, watchers: &mut Watchers<'_>) {
        match self {
            IntegerVariableEnum::DomainId(domain_id) => domain_id.unwatch_all(watchers),
            IntegerVariableEnum::Predicate(literal) => literal.unwatch_all(watchers),
        }
    }

    fn watch_all_backtrack(&self, watchers: &mut Watchers<'_>, events: EnumSet<DomainEvent>) {
        match self {
            IntegerVariableEnum::DomainId(domain_id) => {
                domain_id.watch_all_backtrack(watchers, events)
            }
            IntegerVariableEnum::Predicate(literal) => {
                literal.watch_all_backtrack(watchers, events)
            }
        }
    }

    fn unpack_event(&self, event: OpaqueDomainEvent) -> DomainEvent {
        match self {
            IntegerVariableEnum::DomainId(domain_id) => domain_id.unpack_event(event),
            IntegerVariableEnum::Predicate(literal) => literal.unpack_event(event),
        }
    }

    fn get_holes_at_current_checkpoint(
        &self,
        assignments: &Assignments,
    ) -> impl Iterator<Item = i32> {
        match self {
            IntegerVariableEnum::DomainId(domain_id) => domain_id
                .get_holes_at_current_checkpoint(assignments)
                .collect::<Vec<_>>()
                .into_iter(),
            IntegerVariableEnum::Predicate(literal) => literal
                .get_holes_at_current_checkpoint(assignments)
                .collect::<Vec<_>>()
                .into_iter(),
        }
    }

    fn get_holes(&self, assignments: &Assignments) -> impl Iterator<Item = i32> {
        match self {
            IntegerVariableEnum::DomainId(domain_id) => domain_id
                .get_holes(assignments)
                .collect::<Vec<_>>()
                .into_iter(),
            IntegerVariableEnum::Predicate(literal) => literal
                .get_holes(assignments)
                .collect::<Vec<_>>()
                .into_iter(),
        }
    }
}

impl CheckerVariable<Predicate> for IntegerVariableEnum {
    fn does_atomic_constrain_self(&self, atomic: &Predicate) -> bool {
        match self {
            IntegerVariableEnum::DomainId(domain_id) => {
                domain_id.does_atomic_constrain_self(atomic)
            }
            IntegerVariableEnum::Predicate(literal) => literal.does_atomic_constrain_self(atomic),
        }
    }

    fn atomic_less_than(&self, value: i32) -> Predicate {
        match self {
            IntegerVariableEnum::DomainId(domain_id) => domain_id.atomic_less_than(value),
            IntegerVariableEnum::Predicate(literal) => literal.atomic_less_than(value),
        }
    }

    fn atomic_greater_than(&self, value: i32) -> Predicate {
        match self {
            IntegerVariableEnum::DomainId(domain_id) => domain_id.atomic_greater_than(value),
            IntegerVariableEnum::Predicate(literal) => literal.atomic_greater_than(value),
        }
    }

    fn atomic_equal(&self, value: i32) -> Predicate {
        match self {
            IntegerVariableEnum::DomainId(domain_id) => domain_id.atomic_equal(value),
            IntegerVariableEnum::Predicate(literal) => literal.atomic_equal(value),
        }
    }

    fn atomic_not_equal(&self, value: i32) -> Predicate {
        match self {
            IntegerVariableEnum::DomainId(domain_id) => domain_id.atomic_not_equal(value),
            IntegerVariableEnum::Predicate(literal) => literal.atomic_not_equal(value),
        }
    }

    fn induced_lower_bound(
        &self,
        variable_state: &pumpkin_checking::VariableState<Predicate>,
    ) -> pumpkin_checking::IntExt {
        match self {
            IntegerVariableEnum::DomainId(domain_id) => {
                domain_id.induced_lower_bound(variable_state)
            }
            IntegerVariableEnum::Predicate(literal) => literal.induced_lower_bound(variable_state),
        }
    }

    fn induced_upper_bound(
        &self,
        variable_state: &pumpkin_checking::VariableState<Predicate>,
    ) -> pumpkin_checking::IntExt {
        match self {
            IntegerVariableEnum::DomainId(domain_id) => {
                domain_id.induced_upper_bound(variable_state)
            }
            IntegerVariableEnum::Predicate(literal) => literal.induced_upper_bound(variable_state),
        }
    }

    fn induced_fixed_value(
        &self,
        variable_state: &pumpkin_checking::VariableState<Predicate>,
    ) -> Option<i32> {
        match self {
            IntegerVariableEnum::DomainId(domain_id) => {
                domain_id.induced_fixed_value(variable_state)
            }
            IntegerVariableEnum::Predicate(literal) => literal.induced_fixed_value(variable_state),
        }
    }

    fn induced_domain_contains(
        &self,
        variable_state: &pumpkin_checking::VariableState<Predicate>,
        value: i32,
    ) -> bool {
        match self {
            IntegerVariableEnum::DomainId(domain_id) => {
                domain_id.induced_domain_contains(variable_state, value)
            }
            IntegerVariableEnum::Predicate(literal) => {
                literal.induced_domain_contains(variable_state, value)
            }
        }
    }

    fn induced_holes<'this, 'state>(
        &'this self,
        variable_state: &'state pumpkin_checking::VariableState<Predicate>,
    ) -> impl Iterator<Item = i32> + 'state
    where
        'this: 'state,
    {
        match self {
            IntegerVariableEnum::DomainId(domain_id) => domain_id
                .induced_holes(variable_state)
                .collect::<Vec<_>>()
                .into_iter(),
            IntegerVariableEnum::Predicate(literal) => literal
                .induced_holes(variable_state)
                .collect::<Vec<_>>()
                .into_iter(),
        }
    }

    fn iter_induced_domain<'this, 'state>(
        &'this self,
        variable_state: &'state pumpkin_checking::VariableState<Predicate>,
    ) -> Option<impl Iterator<Item = i32> + 'state>
    where
        'this: 'state,
    {
        match self {
            IntegerVariableEnum::DomainId(domain_id) => domain_id
                .iter_induced_domain(variable_state)
                .map(|iter| iter.collect::<Vec<_>>().into_iter()),
            IntegerVariableEnum::Predicate(literal) => literal
                .iter_induced_domain(variable_state)
                .map(|iter| iter.collect::<Vec<_>>().into_iter()),
        }
    }
}
