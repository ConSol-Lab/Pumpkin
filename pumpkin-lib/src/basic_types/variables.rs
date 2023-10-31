//! A variable, in the context of the solver, is a view onto a domain. It may forward domain
//! information unaltered, or apply transformations which can be performed without the need of
//! constraints.

use std::cmp::Ordering;

use crate::engine::{
    Delta, DomainChange, DomainEvent, DomainManager, EmptyDomain, OpaqueDomainEvent, Watchers,
};

use super::{DomainId, Predicate, PredicateConstructor};

pub trait IntVar: Clone + PredicateConstructor<Value = i32> {
    /// Get the lower bound of the variable.
    fn lower_bound(&self, domains: &DomainManager) -> i32;

    /// Get the upper bound of the variable.
    fn upper_bound(&self, domains: &DomainManager) -> i32;

    /// Determine whether the value is in the domain of this variable.
    fn contains(&self, domains: &DomainManager, value: i32) -> bool;

    /// Remove a value from the domain of this variable.
    fn remove(&self, domains: &mut DomainManager, value: i32) -> Result<(), EmptyDomain>;

    /// Tighten the lower bound of the domain of this variable.
    fn set_lower_bound(&self, domains: &mut DomainManager, value: i32) -> Result<(), EmptyDomain>;

    /// Tighten the upper bound of the domain of this variable.
    fn set_upper_bound(&self, domains: &mut DomainManager, value: i32) -> Result<(), EmptyDomain>;

    /// Register a watch for this variable on the given domain event.
    fn watch(&self, watchers: &mut Watchers<'_>, event: DomainEvent);

    /// Decode a delta into the change it represents for this variable.
    fn unpack_delta(&self, delta: Delta) -> DomainChange;

    /// Decode a domain event for this variable.
    fn unpack_event(&self, event: OpaqueDomainEvent) -> DomainEvent;

    /// Get a variable which domain is scaled compared to the domain of self.
    ///
    /// The scaled domain will have holes in it. E.g. if we have `dom(x) = {1, 2}`, then
    /// `dom(x.scaled(2)) = {2, 4}` and *not* `dom(x.scaled(2)) = {1, 2, 3, 4}`.
    fn scaled(&self, scale: i32) -> AffineView<Self> {
        AffineView::new(self.clone(), scale, 0)
    }

    /// Get a variable which domain has a constant offset to the domain of self.
    fn offset(&self, offset: i32) -> AffineView<Self> {
        AffineView::new(self.clone(), 1, offset)
    }
}

/// Models the constraint `y = ax + b`, by expressing the domain of `y` as a transformation of the domain of `x`.
#[derive(Clone)]
pub struct AffineView<View> {
    inner: View,
    scale: i32,
    offset: i32,
}

impl IntVar for DomainId {
    fn lower_bound(&self, domains: &DomainManager) -> i32 {
        domains.get_lower_bound(*self)
    }

    fn upper_bound(&self, domains: &DomainManager) -> i32 {
        domains.get_upper_bound(*self)
    }

    fn contains(&self, domains: &DomainManager, value: i32) -> bool {
        domains.is_value_in_domain(*self, value)
    }

    fn remove(&self, domains: &mut DomainManager, value: i32) -> Result<(), EmptyDomain> {
        domains.remove_value_from_domain(*self, value)
    }

    fn set_lower_bound(&self, domains: &mut DomainManager, value: i32) -> Result<(), EmptyDomain> {
        domains.tighten_lower_bound(*self, value)
    }

    fn set_upper_bound(&self, domains: &mut DomainManager, value: i32) -> Result<(), EmptyDomain> {
        domains.tighten_upper_bound(*self, value)
    }

    fn watch(&self, watchers: &mut Watchers<'_>, event: DomainEvent) {
        watchers.watch(*self, event);
    }

    fn unpack_delta(&self, delta: Delta) -> DomainChange {
        delta.unwrap_change()
    }

    fn unpack_event(&self, event: OpaqueDomainEvent) -> DomainEvent {
        event.unwrap()
    }
}

impl<View> AffineView<View> {
    pub fn new(inner: View, scale: i32, offset: i32) -> Self {
        AffineView {
            inner,
            scale,
            offset,
        }
    }

    /// Apply the inverse transformation of this view on a value, to go from the value in the domain
    /// of `self` to a value in the domain of `self.inner`.
    fn invert(&self, value: i32) -> Option<i32> {
        let inverted_translation = value - self.offset;

        if inverted_translation % self.scale == 0 {
            Some(inverted_translation / self.scale)
        } else {
            None
        }
    }

    fn map(&self, value: i32) -> i32 {
        self.scale * value + self.offset
    }
}

impl<View> IntVar for AffineView<View>
where
    View: IntVar,
{
    fn lower_bound(&self, domains: &DomainManager) -> i32 {
        if self.scale < 0 {
            self.map(self.inner.upper_bound(domains))
        } else {
            self.map(self.inner.lower_bound(domains))
        }
    }

    fn upper_bound(&self, domains: &DomainManager) -> i32 {
        if self.scale < 0 {
            self.map(self.inner.lower_bound(domains))
        } else {
            self.map(self.inner.upper_bound(domains))
        }
    }

    fn contains(&self, domains: &DomainManager, value: i32) -> bool {
        self.invert(value)
            .map(|v| self.inner.contains(domains, v))
            .unwrap_or(false)
    }

    fn remove(&self, domains: &mut DomainManager, value: i32) -> Result<(), EmptyDomain> {
        if let Some(v) = self.invert(value) {
            self.inner.remove(domains, v)
        } else {
            Ok(())
        }
    }

    fn set_lower_bound(&self, domains: &mut DomainManager, value: i32) -> Result<(), EmptyDomain> {
        let inverted = self
            .invert(value)
            .expect("handle case where the unscaled value is not integer");

        if self.scale >= 0 {
            self.inner.set_lower_bound(domains, inverted)
        } else {
            self.inner.set_upper_bound(domains, inverted)
        }
    }

    fn set_upper_bound(&self, domains: &mut DomainManager, value: i32) -> Result<(), EmptyDomain> {
        let inverted = self
            .invert(value)
            .expect("handle case where the unscaled value is not integer");

        if self.scale >= 0 {
            self.inner.set_upper_bound(domains, inverted)
        } else {
            self.inner.set_lower_bound(domains, inverted)
        }
    }

    fn watch(&self, watchers: &mut Watchers<'_>, event: DomainEvent) {
        if self.scale.is_negative() {
            match event {
                DomainEvent::LowerBound => self.inner.watch(watchers, DomainEvent::UpperBound),
                DomainEvent::UpperBound => self.inner.watch(watchers, DomainEvent::LowerBound),
                event => self.inner.watch(watchers, event),
            }
        } else {
            self.inner.watch(watchers, event);
        }
    }

    fn unpack_delta(&self, delta: Delta) -> DomainChange {
        match self.inner.unpack_delta(delta) {
            DomainChange::Removal(value) => DomainChange::Removal(self.map(value)),
            DomainChange::LowerBound(lower_bound) => {
                if self.scale >= 0 {
                    DomainChange::LowerBound(self.map(lower_bound))
                } else {
                    DomainChange::UpperBound(self.map(lower_bound))
                }
            }
            DomainChange::UpperBound(upper_bound) => {
                if self.scale >= 0 {
                    DomainChange::UpperBound(self.map(upper_bound))
                } else {
                    DomainChange::LowerBound(self.map(upper_bound))
                }
            }
        }
    }

    fn unpack_event(&self, event: OpaqueDomainEvent) -> DomainEvent {
        if self.scale.is_negative() {
            match self.inner.unpack_event(event) {
                DomainEvent::LowerBound => DomainEvent::UpperBound,
                DomainEvent::UpperBound => DomainEvent::LowerBound,
                event => event,
            }
        } else {
            self.inner.unpack_event(event)
        }
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
        let inverted_bound = self
            .invert(bound)
            .expect("Handle case where invert() returns None.");

        if self.scale < 0 {
            self.inner.upper_bound_predicate(inverted_bound)
        } else {
            self.inner.lower_bound_predicate(inverted_bound)
        }
    }

    fn upper_bound_predicate(&self, bound: Self::Value) -> Predicate {
        let inverted_bound = self
            .invert(bound)
            .expect("Handle case where invert() returns None.");

        if self.scale < 0 {
            self.inner.lower_bound_predicate(inverted_bound)
        } else {
            self.inner.upper_bound_predicate(inverted_bound)
        }
    }

    fn equality_predicate(&self, bound: Self::Value) -> Predicate {
        let inverted_bound = self
            .invert(bound)
            .expect("Handle case where invert() returns None.");
        self.inner.equality_predicate(inverted_bound)
    }

    fn disequality_predicate(&self, bound: Self::Value) -> Predicate {
        let inverted_bound = self
            .invert(bound)
            .expect("Handle case where invert() returns None.");
        self.inner.disequality_predicate(inverted_bound)
    }
}
