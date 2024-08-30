use super::integer_predicate::IntegerPredicate;
use crate::engine::variables::DomainId;
#[cfg(doc)]
use crate::engine::variables::IntegerVariable;
use crate::engine::variables::Literal;
#[cfg(doc)]
use crate::engine::VariableLiteralMappings;

/// This structure is oftentimes used to represent propagations, explanations or decisions. It can
/// either represent an [`IntegerPredicate`], a [`Literal`] which is linked to an
/// [`IntegerPredicate`], or a domain operation which is always true (false) using
/// [`Predicate::True`] ([`Predicate::False`]).
#[derive(Clone, PartialEq, Eq, Copy, Hash)]
pub enum Predicate {
    /// A predicate representing an atomic constraint over an [`IntegerVariable`] (either `[x >=
    /// v]`, `[x <= v]`, `[x == v]`, or `[x != v]`).
    IntegerPredicate(IntegerPredicate),
    /// A predicate represented with a [`Literal`]. Oftentimes this [`Literal`] is internally
    /// linked to an [`IntegerPredicate`].
    Literal(Literal),
    /// A predicate which is always false.
    False,
    /// A predicate which is always true.
    True,
}

impl Predicate {
    /// Returns the [`DomainId`] corresponding with the [`Predicate::IntegerPredicate`]. Returns
    /// [`None`] in the case of [`Predicate::Literal`], [`Predicate::False`], and
    /// [`Predicate::True`].
    pub fn get_domain(&self) -> Option<DomainId> {
        match self {
            Predicate::IntegerPredicate(integer_predicate) => Some(integer_predicate.get_domain()),
            _ => None,
        }
    }

    /// Returns the [`Literal`] corresponding with [`Predicate::Literal`], [`Predicate::False`], and
    /// [`Predicate::True`]. Will return [`None`] in the case of [`Predicate::IntegerPredicate`].
    pub fn get_literal_of_bool_predicate(&self, true_literal: Literal) -> Option<Literal> {
        match self {
            Predicate::IntegerPredicate(_) => None,
            Predicate::Literal(literal) => Some(*literal),
            Predicate::False => Some(!true_literal),
            Predicate::True => Some(true_literal),
        }
    }

    pub fn is_integer_predicate(&self) -> bool {
        matches!(self, Predicate::IntegerPredicate(_))
    }
}

impl std::ops::Not for Predicate {
    type Output = Predicate;
    fn not(self) -> Predicate {
        match self {
            Predicate::IntegerPredicate(integer_predicate) => {
                Predicate::IntegerPredicate(!integer_predicate)
            }
            Predicate::Literal(literal) => Predicate::Literal(!literal),
            Predicate::False => Predicate::True,
            Predicate::True => Predicate::False,
        }
    }
}

impl From<Literal> for Predicate {
    fn from(value: Literal) -> Self {
        Predicate::Literal(value)
    }
}

impl From<IntegerPredicate> for Predicate {
    fn from(value: IntegerPredicate) -> Self {
        Predicate::IntegerPredicate(value)
    }
}

impl std::fmt::Display for Predicate {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Predicate::IntegerPredicate(integer_predicate) => write!(f, "{}", integer_predicate),
            Predicate::Literal(literal) => write!(f, "{}", literal),
            Predicate::False => write!(f, "false"),
            Predicate::True => write!(f, "true"),
        }
    }
}

impl std::fmt::Debug for Predicate {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}
