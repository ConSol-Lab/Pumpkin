use enumset::EnumSet;
use pumpkin_checking::CheckerVariable;

use crate::engine::Assignments;
use crate::engine::notifications::Watchers;
use crate::predicates::Predicate;
use crate::predicates::PredicateConstructor;
use crate::propagation::DomainEvent;
use crate::propagation::OpaqueDomainEvent;
use crate::variables::AffineView;
use crate::variables::DomainId;
use crate::variables::IntegerVariable;
use crate::variables::TransformableVariable;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum AnyInteger {
    DomainId(AffineView<DomainId>),
    Predicate(AffineView<Predicate>),
}

impl From<AffineView<DomainId>> for AnyInteger {
    fn from(value: AffineView<DomainId>) -> Self {
        AnyInteger::DomainId(value)
    }
}

impl From<DomainId> for AnyInteger {
    fn from(value: DomainId) -> Self {
        AnyInteger::DomainId(value.scaled(1))
    }
}

impl From<AffineView<Predicate>> for AnyInteger {
    fn from(value: AffineView<Predicate>) -> Self {
        AnyInteger::Predicate(value)
    }
}

impl From<Predicate> for AnyInteger {
    fn from(value: Predicate) -> Self {
        AnyInteger::Predicate(value.scaled(1))
    }
}

impl PredicateConstructor for AnyInteger {
    type Value = i32;

    fn lower_bound_predicate(&self, bound: Self::Value) -> Predicate {
        match self {
            AnyInteger::DomainId(domain_id) => domain_id.lower_bound_predicate(bound),
            AnyInteger::Predicate(literal) => literal.lower_bound_predicate(bound),
        }
    }

    fn upper_bound_predicate(&self, bound: Self::Value) -> Predicate {
        match self {
            AnyInteger::DomainId(domain_id) => domain_id.upper_bound_predicate(bound),
            AnyInteger::Predicate(literal) => literal.upper_bound_predicate(bound),
        }
    }

    fn equality_predicate(&self, bound: Self::Value) -> Predicate {
        match self {
            AnyInteger::DomainId(domain_id) => domain_id.equality_predicate(bound),
            AnyInteger::Predicate(literal) => literal.equality_predicate(bound),
        }
    }

    fn disequality_predicate(&self, bound: Self::Value) -> Predicate {
        match self {
            AnyInteger::DomainId(domain_id) => domain_id.disequality_predicate(bound),
            AnyInteger::Predicate(literal) => literal.disequality_predicate(bound),
        }
    }
}

impl TransformableVariable<AnyInteger> for AnyInteger {
    fn scaled(&self, scale: i32) -> AnyInteger {
        match self {
            AnyInteger::DomainId(domain_id) => domain_id.scaled(scale).into(),
            AnyInteger::Predicate(literal) => literal.scaled(scale).into(),
        }
    }

    fn offset(&self, offset: i32) -> AnyInteger {
        match self {
            AnyInteger::DomainId(domain_id) => domain_id.offset(offset).into(),
            AnyInteger::Predicate(literal) => literal.offset(offset).into(),
        }
    }
}

impl IntegerVariable for AnyInteger {
    type AffineView = AnyInteger;

    fn lower_bound(&self, assignment: &Assignments) -> i32 {
        match self {
            AnyInteger::DomainId(domain_id) => domain_id.lower_bound(assignment),
            AnyInteger::Predicate(literal) => literal.lower_bound(assignment),
        }
    }

    fn lower_bound_at_trail_position(
        &self,
        assignment: &Assignments,
        trail_position: usize,
    ) -> i32 {
        match self {
            AnyInteger::DomainId(domain_id) => {
                domain_id.lower_bound_at_trail_position(assignment, trail_position)
            }
            AnyInteger::Predicate(literal) => {
                literal.lower_bound_at_trail_position(assignment, trail_position)
            }
        }
    }

    fn upper_bound(&self, assignment: &Assignments) -> i32 {
        match self {
            AnyInteger::DomainId(domain_id) => domain_id.upper_bound(assignment),
            AnyInteger::Predicate(literal) => literal.upper_bound(assignment),
        }
    }

    fn upper_bound_at_trail_position(
        &self,
        assignment: &Assignments,
        trail_position: usize,
    ) -> i32 {
        match self {
            AnyInteger::DomainId(domain_id) => {
                domain_id.upper_bound_at_trail_position(assignment, trail_position)
            }
            AnyInteger::Predicate(literal) => {
                literal.upper_bound_at_trail_position(assignment, trail_position)
            }
        }
    }

    fn contains(&self, assignment: &Assignments, value: i32) -> bool {
        match self {
            AnyInteger::DomainId(domain_id) => domain_id.contains(assignment, value),
            AnyInteger::Predicate(literal) => literal.contains(assignment, value),
        }
    }

    fn contains_at_trail_position(
        &self,
        assignment: &Assignments,
        value: i32,
        trail_position: usize,
    ) -> bool {
        match self {
            AnyInteger::DomainId(domain_id) => {
                domain_id.contains_at_trail_position(assignment, value, trail_position)
            }
            AnyInteger::Predicate(literal) => {
                literal.contains_at_trail_position(assignment, value, trail_position)
            }
        }
    }

    fn iterate_domain(&self, assignment: &Assignments) -> impl Iterator<Item = i32> {
        match self {
            AnyInteger::DomainId(domain_id) => domain_id
                .iterate_domain(assignment)
                .collect::<Vec<_>>()
                .into_iter(),
            AnyInteger::Predicate(literal) => literal
                .iterate_domain(assignment)
                .collect::<Vec<_>>()
                .into_iter(),
        }
    }

    fn watch_all(&self, watchers: &mut Watchers<'_>, events: EnumSet<DomainEvent>) {
        match self {
            AnyInteger::DomainId(domain_id) => domain_id.watch_all(watchers, events),
            AnyInteger::Predicate(literal) => literal.watch_all(watchers, events),
        }
    }

    fn unwatch_all(&self, watchers: &mut Watchers<'_>) {
        match self {
            AnyInteger::DomainId(domain_id) => domain_id.unwatch_all(watchers),
            AnyInteger::Predicate(literal) => literal.unwatch_all(watchers),
        }
    }

    fn watch_all_backtrack(&self, watchers: &mut Watchers<'_>, events: EnumSet<DomainEvent>) {
        match self {
            AnyInteger::DomainId(domain_id) => domain_id.watch_all_backtrack(watchers, events),
            AnyInteger::Predicate(literal) => literal.watch_all_backtrack(watchers, events),
        }
    }

    fn unpack_event(&self, event: OpaqueDomainEvent) -> DomainEvent {
        match self {
            AnyInteger::DomainId(domain_id) => domain_id.unpack_event(event),
            AnyInteger::Predicate(literal) => literal.unpack_event(event),
        }
    }

    fn get_holes_at_current_checkpoint(
        &self,
        assignments: &Assignments,
    ) -> impl Iterator<Item = i32> {
        match self {
            AnyInteger::DomainId(domain_id) => domain_id
                .get_holes_at_current_checkpoint(assignments)
                .collect::<Vec<_>>()
                .into_iter(),
            AnyInteger::Predicate(literal) => literal
                .get_holes_at_current_checkpoint(assignments)
                .collect::<Vec<_>>()
                .into_iter(),
        }
    }

    fn get_holes(&self, assignments: &Assignments) -> impl Iterator<Item = i32> {
        match self {
            AnyInteger::DomainId(domain_id) => domain_id
                .get_holes(assignments)
                .collect::<Vec<_>>()
                .into_iter(),
            AnyInteger::Predicate(literal) => literal
                .get_holes(assignments)
                .collect::<Vec<_>>()
                .into_iter(),
        }
    }
}

impl CheckerVariable<Predicate> for AnyInteger {
    fn does_atomic_constrain_self(&self, atomic: &Predicate) -> bool {
        match self {
            AnyInteger::DomainId(domain_id) => domain_id.does_atomic_constrain_self(atomic),
            AnyInteger::Predicate(literal) => literal.does_atomic_constrain_self(atomic),
        }
    }

    fn atomic_less_than(&self, value: i32) -> Predicate {
        match self {
            AnyInteger::DomainId(domain_id) => domain_id.atomic_less_than(value),
            AnyInteger::Predicate(literal) => literal.atomic_less_than(value),
        }
    }

    fn atomic_greater_than(&self, value: i32) -> Predicate {
        match self {
            AnyInteger::DomainId(domain_id) => domain_id.atomic_greater_than(value),
            AnyInteger::Predicate(literal) => literal.atomic_greater_than(value),
        }
    }

    fn atomic_equal(&self, value: i32) -> Predicate {
        match self {
            AnyInteger::DomainId(domain_id) => domain_id.atomic_equal(value),
            AnyInteger::Predicate(literal) => literal.atomic_equal(value),
        }
    }

    fn atomic_not_equal(&self, value: i32) -> Predicate {
        match self {
            AnyInteger::DomainId(domain_id) => domain_id.atomic_not_equal(value),
            AnyInteger::Predicate(literal) => literal.atomic_not_equal(value),
        }
    }

    fn induced_lower_bound(
        &self,
        variable_state: &pumpkin_checking::VariableState<Predicate>,
    ) -> pumpkin_checking::IntExt {
        match self {
            AnyInteger::DomainId(domain_id) => domain_id.induced_lower_bound(variable_state),
            AnyInteger::Predicate(literal) => literal.induced_lower_bound(variable_state),
        }
    }

    fn induced_upper_bound(
        &self,
        variable_state: &pumpkin_checking::VariableState<Predicate>,
    ) -> pumpkin_checking::IntExt {
        match self {
            AnyInteger::DomainId(domain_id) => domain_id.induced_upper_bound(variable_state),
            AnyInteger::Predicate(literal) => literal.induced_upper_bound(variable_state),
        }
    }

    fn induced_fixed_value(
        &self,
        variable_state: &pumpkin_checking::VariableState<Predicate>,
    ) -> Option<i32> {
        match self {
            AnyInteger::DomainId(domain_id) => domain_id.induced_fixed_value(variable_state),
            AnyInteger::Predicate(literal) => literal.induced_fixed_value(variable_state),
        }
    }

    fn induced_domain_contains(
        &self,
        variable_state: &pumpkin_checking::VariableState<Predicate>,
        value: i32,
    ) -> bool {
        match self {
            AnyInteger::DomainId(domain_id) => {
                domain_id.induced_domain_contains(variable_state, value)
            }
            AnyInteger::Predicate(literal) => {
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
            AnyInteger::DomainId(domain_id) => domain_id
                .induced_holes(variable_state)
                .collect::<Vec<_>>()
                .into_iter(),
            AnyInteger::Predicate(literal) => literal
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
            AnyInteger::DomainId(domain_id) => domain_id
                .iter_induced_domain(variable_state)
                .map(|iter| iter.collect::<Vec<_>>().into_iter()),
            AnyInteger::Predicate(literal) => literal
                .iter_induced_domain(variable_state)
                .map(|iter| iter.collect::<Vec<_>>().into_iter()),
        }
    }
}
