use std::cmp::Ordering;
use std::ops::Add;

use crate::Atomic;

pub trait CheckerVariable<Domains> {
    /// Get the lower bound of a variable.
    fn lower_bound(&self, domains: &Domains) -> I32Ext;

    /// Get the upper bound of a variable.
    fn upper_bound(&self, domains: &Domains) -> I32Ext;

    /// Get the holes within the lower and upper bound of the variable expression.
    fn holes(&self, domains: &Domains) -> impl Iterator<Item = i32> + '_;

    /// Get the fixed value of this variable, if it is fixed.
    fn fixed_value(&self, domains: &Domains) -> Option<i32>;

    /// Obtain an iterator over the domain of the variable.
    ///
    /// If the domain is unbounded, then `None` is returned.
    fn iter_domain(&self, domains: &Domains) -> Option<impl Iterator<Item = i32> + '_>;
}

pub trait CheckerDomains {
    /// Change the domains by applying the given [`Atomic`].
    ///
    /// Returns `true` if the domains remain consistent, or `false` if a domain becomes empty.
    fn apply(&mut self, atomic: Atomic) -> bool;
}

/// An `i32` or infinity.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum I32Ext {
    I32(i32),
    NegativeInf,
    PositiveInf,
}

impl PartialEq<i32> for I32Ext {
    fn eq(&self, other: &i32) -> bool {
        match self {
            I32Ext::I32(v1) => v1 == other,
            I32Ext::NegativeInf | I32Ext::PositiveInf => false,
        }
    }
}

impl PartialOrd<I32Ext> for I32Ext {
    fn partial_cmp(&self, other: &I32Ext) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for I32Ext {
    fn cmp(&self, other: &Self) -> Ordering {
        match self {
            I32Ext::I32(v1) => match other {
                I32Ext::I32(v2) => v1.cmp(v2),
                I32Ext::NegativeInf => Ordering::Greater,
                I32Ext::PositiveInf => Ordering::Less,
            },
            I32Ext::NegativeInf => match other {
                I32Ext::I32(_) => Ordering::Less,
                I32Ext::PositiveInf => Ordering::Less,
                I32Ext::NegativeInf => Ordering::Equal,
            },
            I32Ext::PositiveInf => match other {
                I32Ext::I32(_) => Ordering::Greater,
                I32Ext::NegativeInf => Ordering::Greater,
                I32Ext::PositiveInf => Ordering::Greater,
            },
        }
    }
}

impl PartialOrd<i32> for I32Ext {
    fn partial_cmp(&self, other: &i32) -> Option<Ordering> {
        match self {
            I32Ext::I32(v1) => v1.partial_cmp(other),
            I32Ext::NegativeInf => Some(Ordering::Less),
            I32Ext::PositiveInf => Some(Ordering::Greater),
        }
    }
}

impl Add<i32> for I32Ext {
    type Output = I32Ext;

    fn add(self, rhs: i32) -> Self::Output {
        match self {
            I32Ext::I32(lhs) => I32Ext::I32(lhs + rhs),
            I32Ext::NegativeInf => I32Ext::NegativeInf,
            I32Ext::PositiveInf => I32Ext::PositiveInf,
        }
    }
}

impl Add for I32Ext {
    type Output = I32Ext;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (I32Ext::I32(lhs), I32Ext::I32(rhs)) => I32Ext::I32(lhs + rhs),
            (I32Ext::I32(_), rhs) => rhs,
            (I32Ext::NegativeInf, _) => I32Ext::NegativeInf,
            (I32Ext::PositiveInf, _) => I32Ext::PositiveInf,
        }
    }
}
