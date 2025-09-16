use crate::engine::notifications::DomainEvent;
use crate::engine::notifications::OpaqueDomainEvent;
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
        assignment: &Assignments,
        value: i32,
        _trail_position: usize,
    ) -> bool {
        self.contains(assignment, value)
    }

    fn iterate_domain(&self, _assignment: &Assignments) -> impl Iterator<Item = i32> {
        std::iter::once(*self)
    }

    fn watch_all(
        &self,
        _watchers: &mut crate::engine::notifications::Watchers<'_>,
        _events: enumset::EnumSet<DomainEvent>,
    ) {
    }

    fn watch_all_backtrack(
        &self,
        _watchers: &mut crate::engine::notifications::Watchers<'_>,
        _events: enumset::EnumSet<DomainEvent>,
    ) {
    }

    fn unpack_event(&self, _event: OpaqueDomainEvent) -> DomainEvent {
        unreachable!("A constant should never be able to notify of an event")
    }

    fn get_holes_on_current_decision_level(
        &self,
        _assignments: &Assignments,
    ) -> impl Iterator<Item = i32> {
        std::iter::empty()
    }

    fn get_holes(&self, _assignments: &Assignments) -> impl Iterator<Item = i32> {
        std::iter::empty()
    }
}

impl PredicateConstructor for i32 {
    type Value = i32;

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

    fn equality_predicate(&self, bound: Self::Value) -> Predicate {
        if bound == *self {
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

impl TransformableVariable<i32> for i32 {
    fn scaled(&self, scale: i32) -> i32 {
        *self * scale
    }

    fn offset(&self, offset: i32) -> i32 {
        *self + offset
    }
}
