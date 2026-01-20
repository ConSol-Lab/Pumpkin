use std::fmt::Debug;

use crate::AtomicConstraint;
use crate::Comparison;
use crate::I32Ext;
use crate::TestAtomic;
use crate::VariableState;

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
    fn induced_lower_bound(&self, variable_state: &VariableState<Atomic>) -> I32Ext;

    /// Get the upper bound of the domain.
    fn induced_upper_bound(&self, variable_state: &VariableState<Atomic>) -> I32Ext;

    /// Get the value the variable is fixed to, if the variable is fixed.
    fn induced_fixed_value(&self, variable_state: &VariableState<Atomic>) -> Option<i32>;

    /// Get the holes in the domain.
    fn induced_holes<'this, 'state>(
        &'this self,
        variable_state: &'state VariableState<Atomic>,
    ) -> impl Iterator<Item = i32> + 'state
    where
        'this: 'state;

    /// Iterate the domain of the variable.
    ///
    /// The order of the values is unspecified.
    fn iter_induced_domain<'this, 'state>(
        &'this self,
        variable_state: &'state VariableState<Atomic>,
    ) -> Option<impl Iterator<Item = i32> + 'state>
    where
        'this: 'state;
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

    fn induced_lower_bound(&self, variable_state: &VariableState<TestAtomic>) -> I32Ext {
        variable_state.lower_bound(self)
    }

    fn induced_upper_bound(&self, variable_state: &VariableState<TestAtomic>) -> I32Ext {
        variable_state.upper_bound(self)
    }

    fn induced_fixed_value(&self, variable_state: &VariableState<TestAtomic>) -> Option<i32> {
        variable_state.fixed_value(self)
    }

    fn induced_holes<'this, 'state>(
        &'this self,
        variable_state: &'state VariableState<TestAtomic>,
    ) -> impl Iterator<Item = i32> + 'state
    where
        'this: 'state,
    {
        variable_state.holes(self)
    }

    fn iter_induced_domain<'this, 'state>(
        &'this self,
        variable_state: &'state VariableState<TestAtomic>,
    ) -> Option<impl Iterator<Item = i32> + 'state>
    where
        'this: 'state,
    {
        variable_state.iter_domain(self)
    }
}
