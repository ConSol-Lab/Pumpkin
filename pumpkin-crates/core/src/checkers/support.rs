use std::fmt::Debug;

use crate::containers::HashMap;
use crate::propagation::Domains;
use crate::propagation::LocalId;
use crate::propagation::ReadDomains;
use crate::variables::DomainId;

/// A [`SupportGenerator`] can produce [`Support`]s for values in domains.
pub trait SupportGenerator: Clone + Debug {
    /// The type of value used in the support.
    ///
    /// Depending on how the generator is used, this may be a float or an integer.
    type Value: SupportValue;

    /// Produce a support where the domain corresponding to `local_id` is assigned to `value`.
    ///
    /// The support is written into the `support` buffer. Implementations can assume that this
    /// support is empty when this function is called.
    ///
    /// The support must satisfy the constraint it is supporting, and all assignments must be
    /// within the domain bounds.
    fn support(
        &mut self,
        support: &mut Support<Self::Value>,
        local_id: LocalId,
        value: UnsupportedValue,
        domains: Domains<'_>,
    );

    /// Returns true if the support is a solution to the constraint.
    fn is_solution(&self, support: &Support<Self::Value>) -> bool;
}

/// A value that may be used in a [`Support`].
pub trait SupportValue: Clone + Debug {
    fn is_in(&self, domain: DomainId, domains: Domains<'_>) -> bool;

    /// If the value is an integer, we can cache it to prevent recreating supports for the same
    /// value.
    fn as_int(&self) -> Option<i32>;
}

impl SupportValue for i32 {
    fn is_in(&self, domain: DomainId, domains: Domains<'_>) -> bool {
        domains.contains(&domain, *self)
    }

    fn as_int(&self) -> Option<i32> {
        Some(*self)
    }
}

impl SupportValue for f32 {
    fn is_in(&self, domain: DomainId, domains: Domains<'_>) -> bool {
        let lb = domains.lower_bound(&domain) as f32;
        let ub = domains.upper_bound(&domain) as f32;

        lb <= *self && *self <= ub
    }

    fn as_int(&self) -> Option<i32> {
        if (self.round() - self).abs() < f32::EPSILON {
            Some(self.round() as i32)
        } else {
            None
        }
    }
}

/// An assignment which supports a value for a particular domain.
#[derive(Clone, Debug)]
pub struct Support<Value> {
    assignment: HashMap<DomainId, Value>,
}

impl<Value> Default for Support<Value> {
    fn default() -> Self {
        Self {
            assignment: Default::default(),
        }
    }
}

impl<Value: SupportValue> Support<Value> {
    /// Add a domain assignment to the support.
    ///
    /// Previous assignments of the given domain are overwritten.
    pub fn with_assignment(&mut self, domain_id: DomainId, value: Value) {
        let _ = self.assignment.insert(domain_id, value);
    }

    /// Get the value for the given domain in this support.
    ///
    /// Panics if the domain is unassigned.
    pub fn assignment(&self, domain_id: DomainId) -> Value {
        self.assignment
            .get(&domain_id)
            .cloned()
            .unwrap_or_else(|| panic!("could not get assignment for {domain_id}"))
    }

    /// Drain the support of all its assigned domains.
    ///
    /// Will leave the support empty.
    pub(super) fn drain(&mut self) -> impl ExactSizeIterator<Item = (DomainId, Value)> {
        self.assignment.drain()
    }
}

/// A domain value that needs to be unpacked through [`UnpackUnsupportedValue::unpack`].
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct UnsupportedValue(pub(crate) i32);

pub trait UnpackUnsupportedValue {
    /// Turn the unsupported value into an item in the domain of [`self`] (the variable).
    fn unpack(&self, unsupported_value: UnsupportedValue) -> i32;
}

pub trait SupportsValue<Value = i32>: UnpackUnsupportedValue {
    /// Add the assignment `self = value` to the `support`.
    fn assign(&self, value: Value, support: &mut Support<Value>);

    /// Get the value from the given support.
    ///
    /// Called with the result of [`SupportsValue::assign`].
    ///
    /// Panics if the support has no value for this variable.
    fn support_value(&self, support: &Support<Value>) -> Value;
}

impl UnpackUnsupportedValue for i32 {
    fn unpack(&self, UnsupportedValue(value): UnsupportedValue) -> i32 {
        assert_eq!(value, *self);
        value
    }
}

impl SupportsValue<i32> for i32 {
    fn assign(&self, _: i32, _: &mut Support<i32>) {
        // Do nothing
    }

    fn support_value(&self, _: &Support<i32>) -> i32 {
        *self
    }
}

impl SupportsValue<f32> for i32 {
    fn assign(&self, _: f32, _: &mut Support<f32>) {
        // Do nothing
    }

    fn support_value(&self, _: &Support<f32>) -> f32 {
        *self as f32
    }
}
