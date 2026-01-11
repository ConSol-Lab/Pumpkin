use std::fmt::Display;

/// Captures the data associated with an atomic constraint.
///
/// An atomic constraint has the form `[identifier op value]`, where:
/// - `identifier` identifies a variable,
/// - `op` is a [`Comparison`],
/// - and `value` is an integer.
pub trait AtomicConstraint: Sized {
    /// The type of identifier used for variables.
    type Identifier;

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
