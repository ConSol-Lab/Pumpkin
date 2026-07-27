use std::cmp::Ordering;
use std::fmt::Debug;
use std::iter::Sum;
use std::ops::Add;
use std::ops::AddAssign;
use std::ops::Mul;
use std::ops::Neg;
use std::ops::Sub;

/// An integer or positive/negative infinity.
///
/// # Notes on arithmetic operations:
/// - The result of the operation `infty + -infty` is undetermined, and if evaluated will cause a
///   panic.
/// - Multiplying [`IntExt::PositiveInf`] or [`IntExt::NegativeInf`] with `IntExt::I32(0)` will
///   yield `IntExt::Int(0)`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum IntExt<Int = i32> {
    Int(Int),
    NegativeInf,
    PositiveInf,
}

impl<Int: Copy> IntExt<Int> {
    pub fn as_int(&self) -> Option<Int> {
        match self {
            IntExt::Int(int) => Some(*int),
            IntExt::NegativeInf | IntExt::PositiveInf => None,
        }
    }
}

/// Additional operations on integers.
pub trait NumExt {
    /// Division with rounding up.
    fn div_ceil(self, other: Self) -> Self;

    /// Division with rounding down.
    ///
    /// Note this is different from truncating, which is rounding toward zero.
    fn div_floor(self, other: Self) -> Self;
}

macro_rules! impl_ops {
    ($type:ty) => {
        impl NumExt for $type {
            fn div_ceil(self, other: Self) -> Self {
                // TODO: The source is taken from the standard library nightly implementation of this
                // function and div_floor. Once they are stabilized, these definitions can be removed.
                // Tracking issue: https://github.com/rust-lang/rust/issues/88581
                let d = self / other;
                let r = self % other;
                if (r > 0 && other > 0) || (r < 0 && other < 0) {
                    d + 1
                } else {
                    d
                }
            }

            fn div_floor(self, other: Self) -> Self {
                // TODO: See todo in `div_ceil`.
                let d = self / other;
                let r = self % other;
                if (r > 0 && other < 0) || (r < 0 && other > 0) {
                    d - 1
                } else {
                    d
                }
            }
        }

        impl IntExt<$type> {
            /// Division with rounding _up_, computed exactly with integer arithmetic.
            ///
            /// Returns `None` if both operands are infinite.
            pub fn div_ceil(&self, other: IntExt<$type>) -> Option<IntExt<$type>> {
                use IntExt::*;

                match (*self, other) {
                    (Int(n), Int(d)) => Some(Int(<$type as NumExt>::div_ceil(n, d))),

                    // A finite value divided by an unboundedly large denominator approaches, but for
                    // integers never exceeds, zero.
                    (Int(_), NegativeInf | PositiveInf) => Some(Int(0)),

                    (PositiveInf, Int(d)) => {
                        if d > 0 {
                            Some(PositiveInf)
                        } else {
                            Some(NegativeInf)
                        }
                    }

                    (NegativeInf, Int(d)) => {
                        if d > 0 {
                            Some(NegativeInf)
                        } else {
                            Some(PositiveInf)
                        }
                    }

                    (NegativeInf | PositiveInf, NegativeInf | PositiveInf) => None,
                }
            }

            /// Division with rounding _down_, computed exactly with integer arithmetic.
            ///
            /// Returns `None` if both operands are infinite.
            pub fn div_floor(&self, other: IntExt<$type>) -> Option<IntExt<$type>> {
                use IntExt::*;

                match (*self, other) {
                    (Int(n), Int(d)) => Some(Int(<$type as NumExt>::div_floor(n, d))),

                    (Int(_), NegativeInf | PositiveInf) => Some(Int(0)),

                    (PositiveInf, Int(d)) => {
                        if d > 0 {
                            Some(PositiveInf)
                        } else {
                            Some(NegativeInf)
                        }
                    }

                    (NegativeInf, Int(d)) => {
                        if d > 0 {
                            Some(NegativeInf)
                        } else {
                            Some(PositiveInf)
                        }
                    }

                    (NegativeInf | PositiveInf, NegativeInf | PositiveInf) => None,
                }
            }
        }
    };
}

impl_ops!(i32);
impl_ops!(i64);

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

impl Mul for IntExt<i64> {
    type Output = IntExt<i64>;

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

    #[test]
    fn multiplying_i64s() {
        let a: IntExt<i64> = Int(6);
        let b: IntExt<i64> = Int(-2);
        assert_eq!(a * b, Int(-12));
    }

    #[test]
    fn multiplying_i64_zero_with_infinity_is_zero() {
        let zero: IntExt<i64> = Int(0);
        assert_eq!(zero * IntExt::<i64>::PositiveInf, Int(0));
        assert_eq!(IntExt::<i64>::NegativeInf * zero, Int(0));
    }

    #[test]
    fn multiplying_i64_large_products_do_not_overflow() {
        let a: IntExt<i64> = Int(i32::MAX as i64);
        let b: IntExt<i64> = Int(i32::MAX as i64);
        assert_eq!(a * b, Int(i32::MAX as i64 * i32::MAX as i64));
    }

    #[test]
    fn dividing_i64s_exactly() {
        assert_eq!(Int(7_i64).div_ceil(Int(2)), Some(Int(4)));
        assert_eq!(Int(7_i64).div_floor(Int(2)), Some(Int(3)));
        assert_eq!(Int(-7_i64).div_ceil(Int(2)), Some(Int(-3)));
        assert_eq!(Int(-7_i64).div_floor(Int(2)), Some(Int(-4)));
    }

    #[test]
    fn dividing_i64s_exceeding_f64_precision() {
        // `i32::MAX * i32::MAX` is well past `f64`'s exact-integer range (2^53), so a
        // float-based division would round this incorrectly.
        let huge = i32::MAX as i64 * i32::MAX as i64;
        assert_eq!(
            Int(huge).div_floor(Int(i32::MAX as i64)),
            Some(Int(i32::MAX as i64))
        );
    }

    #[test]
    fn dividing_i64_finite_by_infinite_is_zero() {
        assert_eq!(Int(5_i64).div_ceil(PositiveInf), Some(Int(0)));
        assert_eq!(Int(-5_i64).div_floor(NegativeInf), Some(Int(0)));
    }

    #[test]
    fn dividing_i64_infinite_by_finite_propagates_sign() {
        assert_eq!(
            IntExt::<i64>::PositiveInf.div_ceil(Int(2)),
            Some(PositiveInf)
        );
        assert_eq!(
            IntExt::<i64>::PositiveInf.div_ceil(Int(-2)),
            Some(NegativeInf)
        );
    }

    #[test]
    fn dividing_i64_infinite_by_infinite_is_indeterminate() {
        assert_eq!(IntExt::<i64>::PositiveInf.div_ceil(PositiveInf), None);
        assert_eq!(IntExt::<i64>::NegativeInf.div_floor(PositiveInf), None);
    }
}
