use std::cmp::Ordering;
use std::iter::Sum;
use std::ops::Add;
use std::ops::Mul;
use std::ops::Neg;

/// An [`i32`] or positive/negative infinity.
///
/// # Notes on arithmetic operations:
/// - The result of the operation `infty + -infty` is undetermined, and if evaluated will cause a
///   panic.
/// - Multiplying [`IntExt::PositiveInf`] or [`IntExt::NegativeInf`] with `IntExt::I32(0)` will
///   yield `IntExt::I32(0)`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum IntExt {
    Int(i32),
    NegativeInf,
    PositiveInf,
}

impl From<i32> for IntExt {
    fn from(value: i32) -> Self {
        IntExt::Int(value)
    }
}

impl TryInto<i32> for IntExt {
    type Error = ();

    fn try_into(self) -> Result<i32, Self::Error> {
        match self {
            IntExt::Int(inner) => Ok(inner),
            IntExt::NegativeInf | IntExt::PositiveInf => Err(()),
        }
    }
}

impl PartialEq<i32> for IntExt {
    fn eq(&self, other: &i32) -> bool {
        match self {
            IntExt::Int(v1) => v1 == other,
            IntExt::NegativeInf | IntExt::PositiveInf => false,
        }
    }
}

impl PartialEq<IntExt> for i32 {
    fn eq(&self, other: &IntExt) -> bool {
        other.eq(self)
    }
}

impl PartialOrd<IntExt> for i32 {
    fn partial_cmp(&self, other: &IntExt) -> Option<Ordering> {
        other.neg().partial_cmp(&self.neg())
    }
}

impl PartialOrd<IntExt> for IntExt {
    fn partial_cmp(&self, other: &IntExt) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for IntExt {
    fn cmp(&self, other: &Self) -> Ordering {
        match self {
            IntExt::Int(v1) => match other {
                IntExt::Int(v2) => v1.cmp(v2),
                IntExt::NegativeInf => Ordering::Greater,
                IntExt::PositiveInf => Ordering::Less,
            },
            IntExt::NegativeInf => match other {
                IntExt::Int(_) => Ordering::Less,
                IntExt::PositiveInf => Ordering::Less,
                IntExt::NegativeInf => Ordering::Equal,
            },
            IntExt::PositiveInf => match other {
                IntExt::Int(_) => Ordering::Greater,
                IntExt::NegativeInf => Ordering::Greater,
                IntExt::PositiveInf => Ordering::Greater,
            },
        }
    }
}

impl PartialOrd<i32> for IntExt {
    fn partial_cmp(&self, other: &i32) -> Option<Ordering> {
        match self {
            IntExt::Int(v1) => v1.partial_cmp(other),
            IntExt::NegativeInf => Some(Ordering::Less),
            IntExt::PositiveInf => Some(Ordering::Greater),
        }
    }
}

impl Add<i32> for IntExt {
    type Output = IntExt;

    fn add(self, rhs: i32) -> Self::Output {
        self + IntExt::Int(rhs)
    }
}

impl Add for IntExt {
    type Output = IntExt;

    fn add(self, rhs: IntExt) -> Self::Output {
        match (self, rhs) {
            (IntExt::Int(lhs), IntExt::Int(rhs)) => IntExt::Int(lhs + rhs),

            (IntExt::Int(_), Self::NegativeInf) => Self::NegativeInf,
            (IntExt::Int(_), Self::PositiveInf) => Self::PositiveInf,
            (Self::NegativeInf, IntExt::Int(_)) => Self::NegativeInf,
            (Self::PositiveInf, IntExt::Int(_)) => Self::PositiveInf,

            (IntExt::NegativeInf, IntExt::NegativeInf) => IntExt::NegativeInf,
            (IntExt::PositiveInf, IntExt::PositiveInf) => IntExt::PositiveInf,

            (lhs @ IntExt::NegativeInf, rhs @ IntExt::PositiveInf)
            | (lhs @ IntExt::PositiveInf, rhs @ IntExt::NegativeInf) => {
                panic!("the result of {lhs:?} + {rhs:?} is indeterminate")
            }
        }
    }
}

impl Mul<i32> for IntExt {
    type Output = IntExt;

    fn mul(self, rhs: i32) -> Self::Output {
        self * IntExt::Int(rhs)
    }
}

impl Mul for IntExt {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (IntExt::Int(lhs), IntExt::Int(rhs)) => IntExt::Int(lhs * rhs),

            // Multiplication with 0 will always yield 0.
            (IntExt::Int(0), Self::NegativeInf)
            | (IntExt::Int(0), Self::PositiveInf)
            | (Self::NegativeInf, IntExt::Int(0))
            | (Self::PositiveInf, IntExt::Int(0)) => IntExt::Int(0),

            (IntExt::Int(value), IntExt::NegativeInf)
            | (IntExt::NegativeInf, IntExt::Int(value)) => {
                if value >= 0 {
                    IntExt::NegativeInf
                } else {
                    IntExt::PositiveInf
                }
            }

            (IntExt::Int(value), IntExt::PositiveInf)
            | (IntExt::PositiveInf, IntExt::Int(value)) => {
                if value >= 0 {
                    IntExt::PositiveInf
                } else {
                    IntExt::NegativeInf
                }
            }

            (IntExt::NegativeInf, IntExt::NegativeInf)
            | (IntExt::PositiveInf, IntExt::PositiveInf) => IntExt::PositiveInf,

            (IntExt::NegativeInf, IntExt::PositiveInf)
            | (IntExt::PositiveInf, IntExt::NegativeInf) => IntExt::NegativeInf,
        }
    }
}

impl Neg for IntExt {
    type Output = Self;

    fn neg(self) -> Self::Output {
        match self {
            IntExt::Int(value) => IntExt::Int(-value),
            IntExt::NegativeInf => IntExt::PositiveInf,
            IntExt::PositiveInf => Self::NegativeInf,
        }
    }
}

impl Sum for IntExt {
    fn sum<I: Iterator<Item = Self>>(iter: I) -> Self {
        iter.fold(IntExt::Int(0), |acc, value| acc + value)
    }
}

#[cfg(test)]
mod tests {
    use IntExt::*;

    use super::*;

    #[test]
    fn ordering_of_i32_with_i32_ext() {
        assert!(Int(2) < 3);
        assert!(Int(-1) < 3);
        assert!(Int(-10) < -1);
    }

    #[test]
    fn ordering_of_i32_ext_with_i32() {
        assert!(1 < Int(2));
        assert!(-10 < Int(-1));
        assert!(-11 < Int(-10));
    }

    #[test]
    fn test_adding_i32s() {
        assert_eq!(Int(3) + Int(4), Int(7));
    }

    #[test]
    fn test_adding_negative_inf() {
        assert_eq!(Int(3) + NegativeInf, NegativeInf);
    }

    #[test]
    fn test_adding_positive_inf() {
        assert_eq!(Int(3) + PositiveInf, PositiveInf);
    }
}
