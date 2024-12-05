use std::{
    fmt::{Debug, Display},
    ops::Not,
};

use crate::engine::variables::DomainId;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum Comparator {
    LessEqual,
    GreaterEqual,
    NotEqual,
    Equal,
}

impl Comparator {
    pub fn opposite(self) -> Comparator {
        use Comparator::*;

        match self {
            LessEqual => GreaterEqual,
            GreaterEqual => LessEqual,
            NotEqual => Equal,
            Equal => NotEqual,
        }
    }
}

impl Debug for Comparator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Comparator::*;

        let string = match self {
            LessEqual => "<=",
            GreaterEqual => ">=",
            NotEqual => "!=",
            Equal => "==",
        };

        write!(f, "{string}")
    }
}

impl Display for Comparator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

/// Representation of a domain operation.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Atom {
    pub domain_id: DomainId,
    pub comparator: Comparator,
    pub value: i32,
}

impl Not for Atom {
    type Output = Atom;

    fn not(self) -> Self::Output {
        use Comparator::*;

        let Atom {
            domain_id,
            comparator,
            value,
        } = self;

        let not_comparator = comparator.opposite();

        let not_value = match comparator {
            LessEqual => value + 1,
            GreaterEqual => value - 1,
            Equal | NotEqual => value,
        };

        Atom {
            domain_id,
            comparator: not_comparator,
            value: not_value,
        }
    }
}

impl Atom {
    /// Returns true if `self /\ other -> false`
    pub fn is_mutually_exclusive(&self, other: Atom) -> bool {
        if self.domain_id != other.domain_id {
            // Atoms over different domains are never mutually exclusive.
            return false;
        }

        match (self.comparator, other.comparator) {
            (Comparator::GreaterEqual, Comparator::GreaterEqual)
            | (Comparator::GreaterEqual, Comparator::NotEqual)
            | (Comparator::LessEqual, Comparator::LessEqual)
            | (Comparator::LessEqual, Comparator::NotEqual)
            | (Comparator::NotEqual, Comparator::GreaterEqual)
            | (Comparator::NotEqual, Comparator::LessEqual)
            | (Comparator::NotEqual, Comparator::NotEqual) => false,

            (Comparator::GreaterEqual, Comparator::LessEqual)
            | (Comparator::GreaterEqual, Comparator::Equal)
            | (Comparator::Equal, Comparator::LessEqual) => self.value > other.value,

            (Comparator::LessEqual, Comparator::GreaterEqual)
            | (Comparator::LessEqual, Comparator::Equal)
            | (Comparator::Equal, Comparator::GreaterEqual) => self.value < other.value,

            (Comparator::Equal, Comparator::NotEqual)
            | (Comparator::NotEqual, Comparator::Equal) => self.value == other.value,

            (Comparator::Equal, Comparator::Equal) => self.value != other.value,
        }
    }
}

impl Display for Atom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Atom {
            domain_id,
            comparator,
            value,
        } = self;

        write!(f, "[{domain_id} {comparator} {value}]")
    }
}

impl Debug for Atom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

#[derive(Clone, PartialEq, Eq, Copy, Hash)]
pub enum Predicate {
    True,
    False,
    Atom(Atom),
}

impl Display for Predicate {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Predicate::True => write!(f, "true"),
            Predicate::False => write!(f, "false"),
            Predicate::Atom(atom) => write!(f, "{atom}"),
        }
    }
}

impl Debug for Predicate {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

impl From<Atom> for Predicate {
    fn from(value: Atom) -> Self {
        Predicate::Atom(value)
    }
}

impl Predicate {
    /// Returns true if `self /\ other -> false`
    pub(crate) fn is_mutually_exclusive_with(self, other: Predicate) -> bool {
        match (self, other) {
            (Predicate::Atom(atom), Predicate::Atom(other)) => atom.is_mutually_exclusive(other),
            (s, o) => todo!("implement mutual exclusivity for {s} and {o}"),
        }
    }

    pub fn is_equality_predicate(&self) -> bool {
        matches!(
            self,
            Predicate::Atom(Atom {
                comparator: Comparator::Equal,
                ..
            })
        )
    }

    pub fn is_lower_bound_predicate(&self) -> bool {
        matches!(
            self,
            Predicate::Atom(Atom {
                comparator: Comparator::GreaterEqual,
                ..
            })
        )
    }

    pub fn is_upper_bound_predicate(&self) -> bool {
        matches!(
            self,
            Predicate::Atom(Atom {
                comparator: Comparator::LessEqual,
                ..
            })
        )
    }

    pub fn is_not_equal_predicate(&self) -> bool {
        matches!(
            self,
            Predicate::Atom(Atom {
                comparator: Comparator::NotEqual,
                ..
            })
        )
    }

    /// Returns the [`DomainId`] of the [`Predicate`]
    pub fn get_domain(&self) -> Option<DomainId> {
        match self {
            Predicate::True | Predicate::False => None,
            Predicate::Atom(atom) => Some(atom.domain_id),
        }
    }

    pub fn get_right_hand_side(&self) -> Option<i32> {
        match self {
            Predicate::True | Predicate::False => None,
            Predicate::Atom(atom) => Some(atom.value),
        }
    }

    pub fn trivially_true() -> Predicate {
        Predicate::True
    }

    pub fn trivially_false() -> Predicate {
        Predicate::False
    }
}

impl Not for Predicate {
    type Output = Predicate;

    fn not(self) -> Self::Output {
        match self {
            Predicate::True => Predicate::False,
            Predicate::False => Predicate::True,
            Predicate::Atom(atom) => Predicate::Atom(!atom),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::{predicate, variables::DomainId};

    use super::Predicate;

    #[test]
    fn negating_trivially_true_predicate() {
        let trivially_true = Predicate::trivially_true();
        let trivially_false = Predicate::trivially_false();
        assert!(!trivially_true == trivially_false);
    }

    #[test]
    fn negating_trivially_false_predicate() {
        let trivially_true = Predicate::trivially_true();
        let trivially_false = Predicate::trivially_false();
        assert!(!trivially_false == trivially_true);
    }

    #[test]
    fn mutual_exclusivity_of_upper_bound_and_lower_bound() {
        let dom = DomainId { id: 1 };

        let p1 = predicate![dom <= 4];
        let p2 = predicate![dom >= 5];

        assert!(p1.is_mutually_exclusive_with(p2));
    }

    #[test]
    fn mutual_exclusivity_of_upper_bound_and_equality() {
        let dom = DomainId { id: 1 };

        let p1 = predicate![dom <= 4];
        let p2 = predicate![dom == 5];

        assert!(p1.is_mutually_exclusive_with(p2));
    }

    #[test]
    fn mutual_exclusivity_of_lower_bound_and_equality() {
        let dom = DomainId { id: 1 };

        let p1 = predicate![dom >= 6];
        let p2 = predicate![dom == 5];

        assert!(p1.is_mutually_exclusive_with(p2));
    }
}
