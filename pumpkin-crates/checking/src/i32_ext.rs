use std::cmp::Ordering;
use std::iter::Sum;
use std::ops::Add;
use std::ops::Mul;

/// An [`i32`] or positive/negative infinity.
///
/// # Note
/// The result of the operation `infty + -infty` is undetermined, and if evaluated will cause a
/// panic.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum I32Ext {
    I32(i32),
    NegativeInf,
    PositiveInf,
}

impl From<i32> for I32Ext {
    fn from(value: i32) -> Self {
        I32Ext::I32(value)
    }
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

impl Add for I32Ext {
    type Output = I32Ext;

    fn add(self, rhs: I32Ext) -> Self::Output {
        match (self, rhs) {
            (I32Ext::I32(lhs), I32Ext::I32(rhs)) => I32Ext::I32(lhs + rhs),

            (I32Ext::I32(_), Self::NegativeInf) => Self::NegativeInf,
            (I32Ext::I32(_), Self::PositiveInf) => Self::PositiveInf,
            (Self::NegativeInf, I32Ext::I32(_)) => Self::NegativeInf,
            (Self::PositiveInf, I32Ext::I32(_)) => Self::PositiveInf,

            (I32Ext::NegativeInf, I32Ext::NegativeInf) => I32Ext::NegativeInf,
            (I32Ext::PositiveInf, I32Ext::PositiveInf) => I32Ext::PositiveInf,

            (lhs @ I32Ext::NegativeInf, rhs @ I32Ext::PositiveInf)
            | (lhs @ I32Ext::PositiveInf, rhs @ I32Ext::NegativeInf) => {
                panic!("the result of {lhs:?} + {rhs:?} is indeterminate")
            }
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

impl Mul<i32> for I32Ext {
    type Output = I32Ext;

    fn mul(self, rhs: i32) -> Self::Output {
        match self {
            I32Ext::I32(lhs) => I32Ext::I32(lhs * rhs),
            I32Ext::NegativeInf => {
                if rhs >= 0 {
                    I32Ext::NegativeInf
                } else {
                    I32Ext::PositiveInf
                }
            }
            I32Ext::PositiveInf => {
                if rhs >= 0 {
                    I32Ext::PositiveInf
                } else {
                    I32Ext::NegativeInf
                }
            }
        }
    }
}

impl Sum for I32Ext {
    fn sum<I: Iterator<Item = Self>>(iter: I) -> Self {
        iter.fold(I32Ext::I32(0), |acc, value| acc + value)
    }
}

#[cfg(test)]
mod tests {
    use I32Ext::*;

    use super::*;

    #[test]
    fn test_adding_i32s() {
        assert_eq!(I32(3) + I32(4), I32(7));
    }

    #[test]
    fn test_adding_negative_inf() {
        assert_eq!(I32(3) + NegativeInf, NegativeInf);
    }

    #[test]
    fn test_adding_positive_inf() {
        assert_eq!(I32(3) + PositiveInf, PositiveInf);
    }
}
