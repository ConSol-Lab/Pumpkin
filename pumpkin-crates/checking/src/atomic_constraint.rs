use std::fmt::Debug;
use std::fmt::Display;
use std::hash::Hash;

/// Captures the data associated with an atomic constraint.
///
/// An atomic constraint has the form `[identifier op value]`, where:
/// - `identifier` identifies a variable,
/// - `op` is a [`Comparison`],
/// - and `value` is an integer.
pub trait AtomicConstraint: Clone + Debug + Sized {
    /// The type of identifier used for variables.
    type Identifier: Hash + Eq;

    /// The identifier of this atomic constraint.
    fn identifier(&self) -> Self::Identifier;

    /// The [`Comparison`] used for this atomic constraint.
    fn comparison(&self) -> Comparison;

    /// The value on the right-hand side of this atomic constraint.
    fn value(&self) -> i32;

    /// The strongest atomic constraint that is mutually exclusive with self.
    fn negate(&self) -> Self;
}

/// An arithmetic comparison between two integers.
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

/// A simple implementation of an [`AtomicConstraint`].
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct TestAtomic {
    pub name: &'static str,
    pub comparison: Comparison,
    pub value: i32,
}

impl AtomicConstraint for TestAtomic {
    type Identifier = &'static str;

    fn identifier(&self) -> Self::Identifier {
        self.name
    }

    fn comparison(&self) -> Comparison {
        self.comparison
    }

    fn value(&self) -> i32 {
        self.value
    }

    fn negate(&self) -> Self {
        TestAtomic {
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
                Comparison::NotEqual | Comparison::Equal => self.value,
            },
        }
    }
}

/// Create a [`TestAtomic`] using a DSL.
///
/// # Example
/// ```
/// pumpkin_checking::test_atomic!([x >= 5]);
/// pumpkin_checking::test_atomic!([y != 10]);
/// ```
#[macro_export]
macro_rules! test_atomic {
    (@to_comparison >=) => {
        $crate::Comparison::GreaterEqual
    };
    (@to_comparison <=) => {
        $crate::Comparison::LessEqual
    };
    (@to_comparison ==) => {
        $crate::Comparison::Equal
    };
    (@to_comparison !=) => {
        $crate::Comparison::NotEqual
    };

    ([$name:ident $comp:tt $value:expr]) => {
        $crate::TestAtomic {
            name: stringify!($name),
            comparison: $crate::test_atomic!(@to_comparison $comp),
            value: $value,
        }
    };
}
