use std::cmp::Ordering;

use enumset::EnumSet;

use super::TransformableVariable;
use crate::engine::opaque_domain_event::OpaqueDomainEvent;
use crate::engine::predicates::predicate::Predicate;
use crate::engine::predicates::predicate_constructor::PredicateConstructor;
use crate::engine::reason::ReasonRef;
use crate::engine::variables::DomainId;
use crate::engine::variables::IntegerVariable;
use crate::engine::Assignments;
use crate::engine::EmptyDomain;
use crate::engine::IntDomainEvent;
use crate::engine::Watchers;
use crate::math::num_ext::NumExt;

/// Models the constraint `y = ax + b`, by expressing the domain of `y` as a transformation of the
/// domain of `x`.
#[derive(Clone, Copy, Hash, Eq, PartialEq)]
pub struct AffineView<Inner> {
    // HACK
    pub(crate) inner: Inner,
    scale: i32,
    offset: i32,
}

impl<Inner> AffineView<Inner> {
    pub fn new(inner: Inner, scale: i32, offset: i32) -> Self {
        AffineView {
            inner,
            scale,
            offset,
        }
    }

    /// Apply the inverse transformation of this view on a value, to go from the value in the domain
    /// of `self` to a value in the domain of `self.inner`.
    fn invert(&self, value: i32, rounding: Rounding) -> i32 {
        let inverted_translation = value - self.offset;

        match rounding {
            Rounding::Up => <i32 as NumExt>::div_ceil(inverted_translation, self.scale),
            Rounding::Down => <i32 as NumExt>::div_floor(inverted_translation, self.scale),
        }
    }

    fn map(&self, value: i32) -> i32 {
        self.scale * value + self.offset
    }
}

impl<View> IntegerVariable for AffineView<View>
where
    View: IntegerVariable,
{
    type AffineView = Self;

    fn lower_bound(&self, assignment: &Assignments) -> i32 {
        if self.scale < 0 {
            self.map(self.inner.upper_bound(assignment))
        } else {
            self.map(self.inner.lower_bound(assignment))
        }
    }

    fn lower_bound_at_trail_position(
        &self,
        assignment: &Assignments,
        trail_position: usize,
    ) -> i32 {
        if self.scale < 0 {
            self.map(
                self.inner
                    .upper_bound_at_trail_position(assignment, trail_position),
            )
        } else {
            self.map(
                self.inner
                    .lower_bound_at_trail_position(assignment, trail_position),
            )
        }
    }

    fn upper_bound(&self, assignment: &Assignments) -> i32 {
        if self.scale < 0 {
            self.map(self.inner.lower_bound(assignment))
        } else {
            self.map(self.inner.upper_bound(assignment))
        }
    }

    fn upper_bound_at_trail_position(
        &self,
        assignment: &Assignments,
        trail_position: usize,
    ) -> i32 {
        if self.scale < 0 {
            self.map(
                self.inner
                    .lower_bound_at_trail_position(assignment, trail_position),
            )
        } else {
            self.map(
                self.inner
                    .upper_bound_at_trail_position(assignment, trail_position),
            )
        }
    }

    fn contains(&self, assignment: &Assignments, value: i32) -> bool {
        if (value - self.offset) % self.scale == 0 {
            let inverted = self.invert(value, Rounding::Up);
            self.inner.contains(assignment, inverted)
        } else {
            false
        }
    }

    fn contains_at_trail_position(
        &self,
        assignment: &Assignments,
        value: i32,
        trail_position: usize,
    ) -> bool {
        if (value - self.offset) % self.scale == 0 {
            let inverted = self.invert(value, Rounding::Up);
            self.inner
                .contains_at_trail_position(assignment, inverted, trail_position)
        } else {
            false
        }
    }

    fn iterate_domain(&self, assignment: &Assignments) -> impl Iterator<Item = i32> {
        self.inner
            .iterate_domain(assignment)
            .map(|value| self.map(value))
    }

    fn remove(
        &self,
        assignment: &mut Assignments,
        value: i32,
        reason: Option<ReasonRef>,
    ) -> Result<(), EmptyDomain> {
        if (value - self.offset) % self.scale == 0 {
            let inverted = self.invert(value, Rounding::Up);
            self.inner.remove(assignment, inverted, reason)
        } else {
            Ok(())
        }
    }

    fn set_lower_bound(
        &self,
        assignment: &mut Assignments,
        value: i32,
        reason: Option<ReasonRef>,
    ) -> Result<(), EmptyDomain> {
        if self.scale >= 0 {
            let inverted = self.invert(value, Rounding::Up);
            self.inner.set_lower_bound(assignment, inverted, reason)
        } else {
            let inverted = self.invert(value, Rounding::Down);
            self.inner.set_upper_bound(assignment, inverted, reason)
        }
    }

    fn set_upper_bound(
        &self,
        assignment: &mut Assignments,
        value: i32,
        reason: Option<ReasonRef>,
    ) -> Result<(), EmptyDomain> {
        if self.scale >= 0 {
            let inverted = self.invert(value, Rounding::Down);
            self.inner.set_upper_bound(assignment, inverted, reason)
        } else {
            let inverted = self.invert(value, Rounding::Up);
            self.inner.set_lower_bound(assignment, inverted, reason)
        }
    }

    fn watch_all(&self, watchers: &mut Watchers<'_>, mut events: EnumSet<IntDomainEvent>) {
        let bound = IntDomainEvent::LowerBound | IntDomainEvent::UpperBound;
        let intersection = events.intersection(bound);
        if intersection.len() == 1 && self.scale.is_negative() {
            events = events.symmetrical_difference(bound);
        }
        self.inner.watch_all(watchers, events);
    }

    fn watch_all_backtrack(
        &self,
        watchers: &mut Watchers<'_>,
        mut events: EnumSet<IntDomainEvent>,
    ) {
        let bound = IntDomainEvent::LowerBound | IntDomainEvent::UpperBound;
        let intersection = events.intersection(bound);
        if intersection.len() == 1 && self.scale.is_negative() {
            events = events.symmetrical_difference(bound);
        }
        self.inner.watch_all_backtrack(watchers, events);
    }

    fn unpack_event(&self, event: OpaqueDomainEvent) -> IntDomainEvent {
        if self.scale.is_negative() {
            match self.inner.unpack_event(event) {
                IntDomainEvent::LowerBound => IntDomainEvent::UpperBound,
                IntDomainEvent::UpperBound => IntDomainEvent::LowerBound,
                event => event,
            }
        } else {
            self.inner.unpack_event(event)
        }
    }
}

impl<View> TransformableVariable<AffineView<View>> for AffineView<View>
where
    View: IntegerVariable,
{
    fn scaled(&self, scale: i32) -> AffineView<View> {
        let mut result = self.clone();
        result.scale *= scale;
        result.offset *= scale;
        result
    }

    fn offset(&self, offset: i32) -> AffineView<View> {
        let mut result = self.clone();
        result.offset += offset;
        result
    }
}

impl<Var: std::fmt::Debug> std::fmt::Debug for AffineView<Var> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.scale == -1 {
            write!(f, "-")?;
        } else if self.scale != 1 {
            write!(f, "{} * ", self.scale)?;
        }

        write!(f, "({:?})", self.inner)?;

        match self.offset.cmp(&0) {
            Ordering::Less => write!(f, " - {}", -self.offset)?,
            Ordering::Equal => {}
            Ordering::Greater => write!(f, " + {}", self.offset)?,
        }

        Ok(())
    }
}

impl<Var: PredicateConstructor<Value = i32>> PredicateConstructor for AffineView<Var> {
    type Value = Var::Value;

    fn lower_bound_predicate(&self, bound: Self::Value) -> Predicate {
        if self.scale < 0 {
            let inverted_bound = self.invert(bound, Rounding::Down);
            self.inner.upper_bound_predicate(inverted_bound)
        } else {
            let inverted_bound = self.invert(bound, Rounding::Up);
            self.inner.lower_bound_predicate(inverted_bound)
        }
    }

    fn upper_bound_predicate(&self, bound: Self::Value) -> Predicate {
        if self.scale < 0 {
            let inverted_bound = self.invert(bound, Rounding::Up);
            self.inner.lower_bound_predicate(inverted_bound)
        } else {
            let inverted_bound = self.invert(bound, Rounding::Down);
            self.inner.upper_bound_predicate(inverted_bound)
        }
    }

    fn equality_predicate(&self, bound: Self::Value) -> Predicate {
        if (bound - self.offset) % self.scale == 0 {
            let inverted_bound = self.invert(bound, Rounding::Up);
            self.inner.equality_predicate(inverted_bound)
        } else {
            Predicate::trivially_false()
        }
    }

    fn disequality_predicate(&self, bound: Self::Value) -> Predicate {
        if (bound - self.offset) % self.scale == 0 {
            let inverted_bound = self.invert(bound, Rounding::Up);
            self.inner.disequality_predicate(inverted_bound)
        } else {
            Predicate::trivially_true()
        }
    }
}

impl From<DomainId> for AffineView<DomainId> {
    fn from(value: DomainId) -> Self {
        AffineView::new(value, 1, 0)
    }
}

enum Rounding {
    Up,
    Down,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::predicate;

    #[test]
    fn scaling_an_affine_view() {
        let view = AffineView::new(DomainId::new(0), 3, 4);
        assert_eq!(3, view.scale);
        assert_eq!(4, view.offset);
        let scaled_view = view.scaled(6);
        assert_eq!(18, scaled_view.scale);
        assert_eq!(24, scaled_view.offset);
    }

    #[test]
    fn offsetting_an_affine_view() {
        let view = AffineView::new(DomainId::new(0), 3, 4);
        assert_eq!(3, view.scale);
        assert_eq!(4, view.offset);
        let scaled_view = view.offset(6);
        assert_eq!(3, scaled_view.scale);
        assert_eq!(10, scaled_view.offset);
    }

    #[test]
    fn affine_view_obtaining_a_bound_should_round_optimistically_in_inner_domain() {
        let domain = DomainId::new(0);
        let view = AffineView::new(domain, 2, 0);

        assert_eq!(predicate!(domain >= 1), predicate!(view >= 1));
        assert_eq!(predicate!(domain >= -1), predicate!(view >= -3));
        assert_eq!(predicate!(domain <= 0), predicate!(view <= 1));
        assert_eq!(predicate!(domain <= -3), predicate!(view <= -5));
    }

    #[test]
    fn test_negated_variable_has_bounds_rounded_correctly() {
        let domain = DomainId::new(0);
        let view = AffineView::new(domain, -2, 0);

        assert_eq!(predicate!(view <= -3), predicate!(domain >= 2));
        assert_eq!(predicate!(view >= 5), predicate!(domain <= -3));
    }
}
