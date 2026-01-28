use std::cmp::Ordering;
use std::fmt::Debug;
use std::iter::Sum;
use std::ops::Add;
use std::ops::AddAssign;
use std::ops::Mul;
use std::ops::Neg;
use std::ops::Sub;

/// An [`i32`] or positive/negative infinity.
///
/// # Notes on arithmetic operations:
/// - The result of the operation `infty + -infty` is undetermined, and if evaluated will cause a
///   panic.
/// - Multiplying [`IntExt::PositiveInf`] or [`IntExt::NegativeInf`] with `IntExt::I32(0)` will
///   yield `IntExt::I32(0)`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum IntExt<Int = i32> {
    Int(Int),
    NegativeInf,
    PositiveInf,
}

impl IntExt<i32> {
    pub fn div_ceil(&self, other: IntExt<i32>) -> Option<IntExt<i32>> {
        let result = self.div(other).ceil();

        Self::int_ext_from_int_f64(result)
    }

    pub fn div_floor(&self, other: IntExt<i32>) -> Option<IntExt<i32>> {
        let result = self.div(other).floor();

        Self::int_ext_from_int_f64(result)
    }

    fn int_ext_from_int_f64(value: f64) -> Option<IntExt<i32>> {
        if value.is_nan() {
            return None;
        }

        if value.is_infinite() {
            if value.is_sign_positive() {
                return Some(IntExt::PositiveInf);
            } else {
                return Some(IntExt::NegativeInf);
            }
        }

        assert!(value.fract().abs() < 1e-10);

        Some(IntExt::Int(value as i32))
    }
}

impl<Int: Into<f64>> IntExt<Int> {
    fn div(self, rhs: Self) -> f64 {
        let value: f64 = self.into();
        let rhs_value: f64 = rhs.into();

        value / rhs_value
    }
}

impl<Int: Into<f64>> From<IntExt<Int>> for f64 {
    fn from(value: IntExt<Int>) -> Self {
        match value {
            IntExt::Int(inner) => inner.into(),
            IntExt::NegativeInf => -f64::INFINITY,
            IntExt::PositiveInf => f64::INFINITY,
        }
    }
}

impl From<i32> for IntExt {
    fn from(value: i32) -> Self {
        IntExt::Int(value)
    }
}

impl From<IntExt<i32>> for IntExt<i64> {
    fn from(value: IntExt<i32>) -> Self {
        match value {
            IntExt::Int(int) => IntExt::Int(int.into()),
            IntExt::NegativeInf => IntExt::NegativeInf,
            IntExt::PositiveInf => IntExt::PositiveInf,
        }
    }
}

// TODO: This is not a great pattern, but for now I do not want to touch this.
impl TryInto<i32> for IntExt {
    type Error = ();

    fn try_into(self) -> Result<i32, Self::Error> {
        match self {
            IntExt::Int(inner) => Ok(inner),
            IntExt::NegativeInf | IntExt::PositiveInf => Err(()),
        }
    }
}

impl<Int: PartialEq> PartialEq<Int> for IntExt<Int> {
    fn eq(&self, other: &Int) -> bool {
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

impl<Int: Ord> PartialOrd for IntExt<Int> {
    fn partial_cmp(&self, other: &IntExt<Int>) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<Int: Ord> Ord for IntExt<Int> {
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

impl PartialOrd<i64> for IntExt<i64> {
    fn partial_cmp(&self, other: &i64) -> Option<Ordering> {
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

impl<Int: Add<Output = Int> + Debug> Add for IntExt<Int> {
    type Output = IntExt<Int>;

    fn add(self, rhs: IntExt<Int>) -> Self::Output {
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

impl Sub<IntExt<i64>> for i64 {
    type Output = IntExt<i64>;

    fn sub(self, rhs: IntExt<i64>) -> Self::Output {
        IntExt::Int(self) - rhs
    }
}

impl<Int: Sub<Output = Int> + Debug> Sub for IntExt<Int> {
    type Output = IntExt<Int>;

    fn sub(self, rhs: IntExt<Int>) -> Self::Output {
        match (self, rhs) {
            (IntExt::Int(lhs), IntExt::Int(rhs)) => IntExt::Int(lhs - rhs),

            (IntExt::Int(_), Self::NegativeInf) => Self::PositiveInf,
            (IntExt::Int(_), Self::PositiveInf) => Self::NegativeInf,
            (Self::NegativeInf, IntExt::Int(_)) => Self::NegativeInf,
            (Self::PositiveInf, IntExt::Int(_)) => Self::PositiveInf,

            (lhs @ IntExt::NegativeInf, rhs @ IntExt::NegativeInf)
            | (lhs @ IntExt::PositiveInf, rhs @ IntExt::PositiveInf)
            | (lhs @ IntExt::NegativeInf, rhs @ IntExt::PositiveInf)
            | (lhs @ IntExt::PositiveInf, rhs @ IntExt::NegativeInf) => {
                panic!("the result of {lhs:?} - {rhs:?} is indeterminate")
            }
        }
    }
}

impl<Int> AddAssign<Int> for IntExt<Int>
where
    Int: AddAssign<Int>,
{
    fn add_assign(&mut self, rhs: Int) {
        match self {
            IntExt::Int(value) => {
                value.add_assign(rhs);
            }

            IntExt::NegativeInf | IntExt::PositiveInf => {}
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

impl Sum for IntExt<i64> {
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
