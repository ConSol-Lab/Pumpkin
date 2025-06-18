//! This crate handles the reading and writing of proofs produced by
//! [Pumpkin](https://github.com/consol-lab/pumpkin).
//!
//! For reading/parsing of proofs, use the [`reader`] module. To write proofs to string use the
//! [`writer`] module.

pub mod reader;
pub mod writer;

use std::fmt::Display;
use std::num::NonZero;
use std::ops::Not;
use std::str::FromStr;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum IntComparison {
    GreaterEqual,
    LessEqual,
    Equal,
    NotEqual,
}

impl Display for IntComparison {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            IntComparison::GreaterEqual => ">=",
            IntComparison::LessEqual => "<=",
            IntComparison::Equal => "==",
            IntComparison::NotEqual => "!=",
        };

        write!(f, "{s}")
    }
}

pub trait IntValue: Clone + Display + FromStr + Ord {
    fn increment(&self) -> Self;
    fn decrement(&self) -> Self;
}

macro_rules! impl_int_value {
    ($type:ty) => {
        impl IntValue for $type {
            fn increment(&self) -> Self {
                (*self) + 1
            }

            fn decrement(&self) -> Self {
                (*self) - 1
            }
        }
    };
}

impl_int_value!(i8);
impl_int_value!(i16);
impl_int_value!(i32);
impl_int_value!(i64);

/// An integer atomic constraint of the form `[name op value]`, where `op` is an [`IntComparison`].
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct IntAtomic<Identifier, Int> {
    pub name: Identifier,
    pub comparison: IntComparison,
    pub value: Int,
}

impl<Identifier, Int> IntAtomic<Identifier, Int> {
    /// Create a new integer atomic.
    pub fn new(name: Identifier, comparison: IntComparison, value: Int) -> Self {
        IntAtomic {
            name,
            comparison,
            value,
        }
    }
}

impl<Identifier: Display, Int: Display> Display for IntAtomic<Identifier, Int> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let IntAtomic {
            name,
            comparison,
            value,
        } = self;

        write!(f, "[{name} {comparison} {value}]")
    }
}

impl<Identifier, Int> Not for IntAtomic<Identifier, Int>
where
    Int: IntValue,
{
    type Output = Self;

    fn not(self) -> Self::Output {
        IntAtomic {
            name: self.name,
            comparison: match self.comparison {
                IntComparison::GreaterEqual => IntComparison::LessEqual,
                IntComparison::LessEqual => IntComparison::GreaterEqual,
                IntComparison::Equal => IntComparison::NotEqual,
                IntComparison::NotEqual => IntComparison::Equal,
            },
            value: match self.comparison {
                IntComparison::GreaterEqual => self.value.decrement(),
                IntComparison::LessEqual => self.value.increment(),
                IntComparison::Equal | IntComparison::NotEqual => self.value,
            },
        }
    }
}

/// The ID of a proof step.
pub type ConstraintId = NonZero<u32>;

/// An inference step.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Inference<Identifier, Int, Label> {
    /// The ID of the step.
    pub constraint_id: ConstraintId,
    /// The premises of the proof.
    pub premises: Vec<IntAtomic<Identifier, Int>>,
    /// The consequent of the proof. If this is [`None`], the `premises` explicitly describe a
    /// conflicting assignment.
    pub consequent: Option<IntAtomic<Identifier, Int>>,
    /// The constraint that introduced this inference.
    pub generated_by: Option<ConstraintId>,
    /// The label that identifies the reasoning that introduced this inference.
    pub label: Option<Label>,
}

/// An deduction step.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Deduction<Identifier, Int> {
    /// The ID of the step.
    pub constraint_id: ConstraintId,
    /// The premises of the proof.
    pub premises: Vec<IntAtomic<Identifier, Int>>,
    /// The constraints to apply to derive the deduction. These should point to [`Inference`]s, but
    /// the parser does not verify that.
    pub sequence: Vec<ConstraintId>,
}

/// The conclusion of the proof. This is the final line, and a proof without a conclusion can be
/// considered incomplete. All steps after the conclusion can be ignored.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Conclusion<Identifier, Int> {
    /// The problem is unsatisfiable.
    Unsat,
    /// The proof concludes the given dual bound.
    DualBound(IntAtomic<Identifier, Int>),
}

/// An individual proof step from the proof.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Step<Identifier, Int, Label> {
    Inference(Inference<Identifier, Int, Label>),
    Deduction(Deduction<Identifier, Int>),
    Conclusion(Conclusion<Identifier, Int>),
}
