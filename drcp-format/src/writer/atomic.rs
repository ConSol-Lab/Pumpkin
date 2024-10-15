use std::fmt::Display;

/// An atomic constraint is a constraint on a single CP variable. In this case, the variable is
/// identified through its string name, and can either be a Boolean or Integer variable.
#[derive(Clone, Debug)]
pub enum AtomicConstraint<'a> {
    Bool(&'a str),
    Int(IntAtomicConstraint<'a>),
}

impl Display for AtomicConstraint<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AtomicConstraint::Bool(name) => write!(f, "[{name}]"),
            AtomicConstraint::Int(int_atomic) => write!(f, "{int_atomic}"),
        }
    }
}

/// An atomic constraint over a single integer variable.
#[derive(Clone, Debug)]
pub struct IntAtomicConstraint<'a> {
    /// The name of the variable.
    pub name: &'a str,
    /// The operation on the domain.
    pub comparison: Comparison,
    /// The right-hand side of the comparison.
    pub value: i64,
}

impl Display for IntAtomicConstraint<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let IntAtomicConstraint {
            name,
            comparison,
            value,
        } = self;

        write!(f, "[{name} {comparison} {value}]")
    }
}

#[derive(Clone, Copy, Debug)]
pub enum Comparison {
    GreaterThanEqual,
    LessThanEqual,
    Equal,
    NotEqual,
}

impl Display for Comparison {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let symbol = match self {
            Comparison::GreaterThanEqual => ">=",
            Comparison::LessThanEqual => "<=",
            Comparison::Equal => "==",
            Comparison::NotEqual => "!=",
        };

        write!(f, "{symbol}")
    }
}
