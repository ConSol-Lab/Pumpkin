use std::fmt::Debug;

use crate::AtomicConstraint;
use crate::Comparison;
use crate::DomainView;
use crate::IntExt;
use crate::TestAtomic;

/// A variable in a constraint satisfaction problem.
pub trait CheckerVariable<Atomic: AtomicConstraint>: Debug + Clone {
    /// Tests whether the given atomic is a statement over the variable `self`.
    fn does_atomic_constrain_self(&self, atomic: &Atomic) -> bool;

    /// Get the atomic constraint `[self <= value]`.
    fn atomic_less_than(&self, value: i32) -> Atomic;

    /// Get the atomic constraint `[self <= value]`.
    fn atomic_greater_than(&self, value: i32) -> Atomic;

    /// Get the atomic constraint `[self == value]`.
    fn atomic_equal(&self, value: i32) -> Atomic;

    /// Get the atomic constraint `[self != value]`.
    fn atomic_not_equal(&self, value: i32) -> Atomic;

    /// Get the lower bound of the domain.
    fn induced_lower_bound<View>(&self, domains: &View) -> IntExt
    where
        View: DomainView<Atomic> + ?Sized;

    /// Get the upper bound of the domain.
    fn induced_upper_bound<View>(&self, domains: &View) -> IntExt
    where
        View: DomainView<Atomic> + ?Sized;

    /// Get the value the variable is fixed to, if the variable is fixed.
    fn induced_fixed_value<View>(&self, domains: &View) -> Option<i32>
    where
        View: DomainView<Atomic> + ?Sized;

    /// Returns whether the value is in the domain.
    fn induced_domain_contains<View>(&self, domains: &View, value: i32) -> bool
    where
        View: DomainView<Atomic> + ?Sized;

    /// Get the holes in the domain.
    fn induced_holes<'this, 'state, View>(
        &'this self,
        domains: &'state View,
    ) -> impl Iterator<Item = i32> + 'state
    where
        'this: 'state,
        View: DomainView<Atomic> + ?Sized;

    /// Iterate the domain of the variable.
    ///
    /// The order of the values is unspecified.
    fn iter_induced_domain<'this, 'state, View>(
        &'this self,
        domains: &'state View,
    ) -> Option<impl Iterator<Item = i32> + 'state>
    where
        'this: 'state,
        View: DomainView<Atomic> + ?Sized;
}

impl CheckerVariable<TestAtomic> for &'static str {
    fn does_atomic_constrain_self(&self, atomic: &TestAtomic) -> bool {
        &atomic.name == self
    }

    fn atomic_less_than(&self, value: i32) -> TestAtomic {
        TestAtomic {
            name: self,
            comparison: Comparison::LessEqual,
            value,
        }
    }

    fn atomic_greater_than(&self, value: i32) -> TestAtomic {
        TestAtomic {
            name: self,
            comparison: Comparison::GreaterEqual,
            value,
        }
    }

    fn atomic_equal(&self, value: i32) -> TestAtomic {
        TestAtomic {
            name: self,
            comparison: Comparison::Equal,
            value,
        }
    }

    fn atomic_not_equal(&self, value: i32) -> TestAtomic {
        TestAtomic {
            name: self,
            comparison: Comparison::NotEqual,
            value,
        }
    }

    fn induced_lower_bound<View>(&self, domains: &View) -> IntExt
    where
        View: DomainView<TestAtomic> + ?Sized,
    {
        domains.lower_bound(self)
    }

    fn induced_upper_bound<View>(&self, domains: &View) -> IntExt
    where
        View: DomainView<TestAtomic> + ?Sized,
    {
        domains.upper_bound(self)
    }

    fn induced_fixed_value<View>(&self, domains: &View) -> Option<i32>
    where
        View: DomainView<TestAtomic> + ?Sized,
    {
        domains.fixed_value(self)
    }

    fn induced_domain_contains<View>(&self, domains: &View, value: i32) -> bool
    where
        View: DomainView<TestAtomic> + ?Sized,
    {
        domains.contains(self, value)
    }

    fn induced_holes<'this, 'state, View>(
        &'this self,
        domains: &'state View,
    ) -> impl Iterator<Item = i32> + 'state
    where
        'this: 'state,
        View: DomainView<TestAtomic> + ?Sized,
    {
        domains.holes(self)
    }

    fn iter_induced_domain<'this, 'state, View>(
        &'this self,
        domains: &'state View,
    ) -> Option<impl Iterator<Item = i32> + 'state>
    where
        'this: 'state,
        View: DomainView<TestAtomic> + ?Sized,
    {
        domains.iter_domain(self)
    }
}
