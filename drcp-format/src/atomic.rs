use std::fmt::Display;
use std::ops::Not;

/// An atomic constraint is a constraint on a single CP variable. In this case, the variable is
/// identified through its string name, and can either be a Boolean or Integer variable.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum AtomicConstraint<Identifier> {
    Bool(BoolAtomicConstraint<Identifier>),
    Int(IntAtomicConstraint<Identifier>),
}

impl<Identifier: Display> Display for AtomicConstraint<Identifier> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AtomicConstraint::Bool(bool_atomic) => write!(f, "{bool_atomic}"),
            AtomicConstraint::Int(int_atomic) => write!(f, "{int_atomic}"),
        }
    }
}

impl<Identifier> Not for AtomicConstraint<Identifier> {
    type Output = AtomicConstraint<Identifier>;

    fn not(self) -> Self::Output {
        match self {
            AtomicConstraint::Bool(bool_atomic_constraint) => {
                AtomicConstraint::Bool(!bool_atomic_constraint)
            }
            AtomicConstraint::Int(int_atomic_constraint) => {
                AtomicConstraint::Int(!int_atomic_constraint)
            }
        }
    }
}

/// An atomic constraint over a single boolean variable.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BoolAtomicConstraint<Identifier> {
    /// The name of the variable.
    pub name: Identifier,
    /// The value the boolean variable is equal to.
    pub value: bool,
}

impl<Identifier: Display> Display for BoolAtomicConstraint<Identifier> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{} == {}]", self.name, self.value)
    }
}

impl<Identifier> Not for BoolAtomicConstraint<Identifier> {
    type Output = BoolAtomicConstraint<Identifier>;

    fn not(self) -> Self::Output {
        BoolAtomicConstraint {
            name: self.name,
            value: !self.value,
        }
    }
}

/// An atomic constraint over a single integer variable.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct IntAtomicConstraint<Identifier> {
    /// The name of the variable.
    pub name: Identifier,
    /// The operation on the domain.
    pub comparison: Comparison,
    /// The right-hand side of the comparison.
    pub value: i64,
}

impl<Identifier: Display> Display for IntAtomicConstraint<Identifier> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let IntAtomicConstraint {
            name,
            comparison,
            value,
        } = self;

        write!(f, "[{name} {comparison} {value}]")
    }
}

impl<Identifier> Not for IntAtomicConstraint<Identifier> {
    type Output = IntAtomicConstraint<Identifier>;

    fn not(self) -> Self::Output {
        match self.comparison {
            Comparison::GreaterThanEqual => IntAtomicConstraint {
                name: self.name,
                comparison: Comparison::LessThanEqual,
                value: self.value - 1,
            },
            Comparison::LessThanEqual => IntAtomicConstraint {
                name: self.name,
                comparison: Comparison::GreaterThanEqual,
                value: self.value + 1,
            },
            Comparison::Equal => IntAtomicConstraint {
                name: self.name,
                comparison: Comparison::NotEqual,
                value: self.value,
            },
            Comparison::NotEqual => IntAtomicConstraint {
                name: self.name,
                comparison: Comparison::Equal,
                value: self.value,
            },
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
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
