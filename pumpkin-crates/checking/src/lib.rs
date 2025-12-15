mod variable_state;

use std::fmt::Display;
use std::num::NonZero;
use std::ops::Not;
use std::sync::Arc;

pub use variable_state::*;

pub type ConstraintId = NonZero<u32>;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Comparison {
    GreaterEqual,
    LessEqual,
    Equal,
    NotEqual,
}

impl Display for Comparison {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Comparison::GreaterEqual => ">=",
            Comparison::LessEqual => "<=",
            Comparison::Equal => "==",
            Comparison::NotEqual => "!=",
        };

        write!(f, "{s}")
    }
}

/// An integer atomic constraint of the form `[name op value]`, where `op` is an [`Comparison`].
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Atomic {
    pub name: Arc<str>,
    pub comparison: Comparison,
    pub value: i32,
}

impl Atomic {
    /// Create a new integer atomic.
    pub fn new(name: Arc<str>, comparison: Comparison, value: i32) -> Self {
        Atomic {
            name,
            comparison,
            value,
        }
    }
}

impl Display for Atomic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Atomic {
            name,
            comparison,
            value,
        } = self;

        write!(f, "[{name} {comparison} {value}]")
    }
}

impl Not for Atomic {
    type Output = Self;

    fn not(self) -> Self::Output {
        Atomic {
            name: self.name,
            comparison: match self.comparison {
                Comparison::GreaterEqual => Comparison::LessEqual,
                Comparison::LessEqual => Comparison::GreaterEqual,
                Comparison::Equal => Comparison::NotEqual,
                Comparison::NotEqual => Comparison::Equal,
            },
            value: match self.comparison {
                Comparison::GreaterEqual => self.value - 1,
                Comparison::LessEqual => self.value + 1,
                Comparison::Equal | Comparison::NotEqual => self.value,
            },
        }
    }
}
/// A constraint that is implied by the model.
///
/// Facts are implications `premises -> consequent`, where the consequent can be `false`.
#[derive(Clone, Debug)]
pub struct Fact {
    /// The premises of the fact.
    pub premises: Vec<Atomic>,
    /// The consequent of the fact.
    ///
    /// If this is `None`, then the premises imply false.
    pub consequent: Option<Atomic>,
}

impl Fact {
    /// Create a fact `premises -> false`.
    pub fn nogood(premises: Vec<Atomic>) -> Self {
        Fact {
            premises,
            consequent: None,
        }
    }
}

/// Checker for individual inferences.
pub trait InferenceChecker<Domains> {
    /// Returns true if the given fact is sound according to this checker.
    ///
    /// The domains are set up such that `premises /\ !consequent` are all true.
    ///
    /// Note that the checker does not need to be complete. I.e., the checker may return
    /// `false` even though the fact is sound.
    fn check(&self, domains: Domains, fact: &Fact, constraint_id: ConstraintId) -> bool;
}
