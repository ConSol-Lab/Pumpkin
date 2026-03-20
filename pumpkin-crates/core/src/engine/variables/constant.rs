use pumpkin_checking::CheckerVariable;
use pumpkin_checking::IntExt;

use crate::engine::Assignments;
use crate::predicates::Predicate;
use crate::predicates::PredicateConstructor;
use crate::variables::IntegerVariable;
use crate::variables::TransformableVariable;

impl IntegerVariable for i32 {
    type AffineView = i32;

    fn lower_bound(&self, _assignment: &Assignments) -> i32 {
        *self
    }

    fn lower_bound_at_trail_position(
        &self,
        _assignment: &Assignments,
        _trail_position: usize,
    ) -> i32 {
        *self
    }

    fn upper_bound(&self, _assignment: &Assignments) -> i32 {
        *self
    }

    fn upper_bound_at_trail_position(
        &self,
        _assignment: &Assignments,
        _trail_position: usize,
    ) -> i32 {
        *self
    }

    fn contains(&self, _assignment: &Assignments, value: i32) -> bool {
        value == *self
    }

    fn contains_at_trail_position(
        &self,
        _assignment: &Assignments,
        value: i32,
        _trail_position: usize,
    ) -> bool {
        value == *self
    }

    fn iterate_domain(&self, _assignment: &Assignments) -> impl Iterator<Item = i32> {
        std::iter::once(*self)
    }

    fn watch_all(
        &self,
        _watchers: &mut crate::engine::notifications::Watchers<'_>,
        _events: enumset::EnumSet<crate::propagation::DomainEvent>,
    ) {
    }

    fn unwatch_all(&self, _watchers: &mut crate::engine::notifications::Watchers<'_>) {}

    fn watch_all_backtrack(
        &self,
        _watchers: &mut crate::engine::notifications::Watchers<'_>,
        _events: enumset::EnumSet<crate::propagation::DomainEvent>,
    ) {
    }

    fn unpack_event(
        &self,
        _event: crate::propagation::OpaqueDomainEvent,
    ) -> crate::propagation::DomainEvent {
        unreachable!()
    }

    fn get_holes_at_current_checkpoint(
        &self,
        _assignments: &Assignments,
    ) -> impl Iterator<Item = i32> {
        std::iter::empty()
    }

    fn get_holes(&self, _assignments: &Assignments) -> impl Iterator<Item = i32> {
        std::iter::empty()
    }
}

impl TransformableVariable<i32> for i32 {
    fn scaled(&self, scale: i32) -> i32 {
        *self * scale
    }

    fn offset(&self, offset: i32) -> i32 {
        *self + offset
    }
}

impl PredicateConstructor for i32 {
    type Value = i32;

    fn equality_predicate(&self, bound: Self::Value) -> Predicate {
        if bound == *self {
            Predicate::trivially_true()
        } else {
            Predicate::trivially_false()
        }
    }

    fn lower_bound_predicate(&self, bound: Self::Value) -> Predicate {
        if bound <= *self {
            Predicate::trivially_true()
        } else {
            Predicate::trivially_false()
        }
    }

    fn upper_bound_predicate(&self, bound: Self::Value) -> Predicate {
        if bound >= *self {
            Predicate::trivially_true()
        } else {
            Predicate::trivially_false()
        }
    }

    fn disequality_predicate(&self, bound: Self::Value) -> Predicate {
        if bound != *self {
            Predicate::trivially_true()
        } else {
            Predicate::trivially_false()
        }
    }
}

impl CheckerVariable<Predicate> for i32 {
    fn does_atomic_constrain_self(&self, _atomic: &Predicate) -> bool {
        false
    }

    fn atomic_less_than(&self, value: i32) -> Predicate {
        if value >= *self {
            Predicate::trivially_true()
        } else {
            Predicate::trivially_false()
        }
    }

    fn atomic_greater_than(&self, value: i32) -> Predicate {
        if value <= *self {
            Predicate::trivially_true()
        } else {
            Predicate::trivially_false()
        }
    }

    fn atomic_equal(&self, value: i32) -> Predicate {
        if value == *self {
            Predicate::trivially_true()
        } else {
            Predicate::trivially_false()
        }
    }

    fn atomic_not_equal(&self, value: i32) -> Predicate {
        if value != *self {
            Predicate::trivially_true()
        } else {
            Predicate::trivially_false()
        }
    }

    fn induced_lower_bound(
        &self,
        _variable_state: &pumpkin_checking::VariableState<Predicate>,
    ) -> IntExt {
        IntExt::Int(*self)
    }

    fn induced_upper_bound(
        &self,
        _variable_state: &pumpkin_checking::VariableState<Predicate>,
    ) -> IntExt {
        IntExt::Int(*self)
    }

    fn induced_fixed_value(
        &self,
        _variable_state: &pumpkin_checking::VariableState<Predicate>,
    ) -> Option<i32> {
        Some(*self)
    }

    fn induced_domain_contains(
        &self,
        _variable_state: &pumpkin_checking::VariableState<Predicate>,
        value: i32,
    ) -> bool {
        value == *self
    }

    fn induced_holes<'this, 'state>(
        &'this self,
        _variable_state: &'state pumpkin_checking::VariableState<Predicate>,
    ) -> impl Iterator<Item = i32> + 'state
    where
        'this: 'state,
    {
        std::iter::empty()
    }

    fn iter_induced_domain<'this, 'state>(
        &'this self,
        _variable_state: &'state pumpkin_checking::VariableState<Predicate>,
    ) -> Option<impl Iterator<Item = i32> + 'state>
    where
        'this: 'state,
    {
        Some(std::iter::once(*self))
    }
}
