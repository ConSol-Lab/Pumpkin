use crate::engine::variables::DomainId;
use crate::predicate;

/// Representation of a domain operation.
///
/// It is provided in the form of an atomic constraint over a [`DomainId`]. See the [`predicate!`]
/// macro on how to construct a predicate.
///
/// The 2 most significant bits of the id stored in the [`Predicate`] contains the type of
/// predicate.
#[derive(Clone, PartialEq, Eq, Copy, Hash)]
pub struct Predicate {
    id: u32,
    value: i32,
}

impl Predicate {
    pub(super) fn new(id: u32, value: i32) -> Self {
        Self { id, value }
    }

    fn get_type_code(&self) -> u8 {
        (self.id >> 30) as u8
    }

    pub fn get_predicate_type(&self) -> PredicateType {
        (*self).into()
    }
}

pub(crate) const LOWER_BOUND_CODE: u8 = 1;
pub(crate) const UPPER_BOUND_CODE: u8 = 2;
pub(crate) const EQUALITY_CODE: u8 = 0;
pub(crate) const NOT_EQUALS_CODE: u8 = 3;

#[derive(Debug, Clone, Eq, PartialEq, Copy, Hash)]
pub enum PredicateType {
    LowerBound,
    UpperBound,
    NotEqual,
    Equal,
}

impl From<Predicate> for PredicateType {
    fn from(value: Predicate) -> Self {
        match value.get_type_code() {
            LOWER_BOUND_CODE => Self::LowerBound,
            UPPER_BOUND_CODE => Self::UpperBound,
            EQUALITY_CODE => Self::Equal,
            NOT_EQUALS_CODE => Self::NotEqual,
            code => panic!("Unknown type code {code}"),
        }
    }
}

impl Predicate {
    pub(crate) fn is_mutually_exclusive_with(self, other: Predicate) -> bool {
        let domain_id = self.get_domain();
        let rhs = self.get_right_hand_side();

        let domain_id_other = other.get_domain();
        let rhs_other = other.get_right_hand_side();

        if domain_id != domain_id_other {
            // Domain Ids do not match
            return false;
        }

        match (self.get_predicate_type(), other.get_predicate_type()) {
            (PredicateType::LowerBound, PredicateType::LowerBound)
            | (PredicateType::LowerBound, PredicateType::NotEqual)
            | (PredicateType::UpperBound, PredicateType::UpperBound)
            | (PredicateType::UpperBound, PredicateType::NotEqual)
            | (PredicateType::NotEqual, PredicateType::LowerBound)
            | (PredicateType::NotEqual, PredicateType::UpperBound)
            | (PredicateType::NotEqual, PredicateType::NotEqual) => false,
            (PredicateType::LowerBound, PredicateType::UpperBound) => rhs > rhs_other,
            (PredicateType::UpperBound, PredicateType::LowerBound) => rhs_other > rhs,
            (PredicateType::LowerBound, PredicateType::Equal) => rhs > rhs_other,
            (PredicateType::Equal, PredicateType::LowerBound) => rhs_other > rhs,
            (PredicateType::UpperBound, PredicateType::Equal) => rhs < rhs_other,
            (PredicateType::Equal, PredicateType::UpperBound) => rhs_other < rhs,
            (PredicateType::NotEqual, PredicateType::Equal)
            | (PredicateType::Equal, PredicateType::NotEqual) => rhs == rhs_other,
            (PredicateType::Equal, PredicateType::Equal) => rhs != rhs_other,
        }
    }
    pub fn is_equality_predicate(&self) -> bool {
        self.get_type_code() == EQUALITY_CODE
    }

    pub fn is_lower_bound_predicate(&self) -> bool {
        self.get_type_code() == LOWER_BOUND_CODE
    }

    pub fn is_upper_bound_predicate(&self) -> bool {
        self.get_type_code() == UPPER_BOUND_CODE
    }

    pub fn is_not_equal_predicate(&self) -> bool {
        self.get_type_code() == NOT_EQUALS_CODE
    }

    /// Returns the [`DomainId`] of the [`Predicate`]
    pub fn get_domain(&self) -> DomainId {
        DomainId::new(0b00111111_11111111_11111111_11111111 & self.id)
    }

    pub fn get_right_hand_side(&self) -> i32 {
        self.value
    }

    pub fn trivially_true() -> Predicate {
        // By convention, there is a dummy 0-1 variable set to one at root.
        // We use it to denote the trivially true predicate.
        let domain_id = DomainId { id: 0 };
        predicate!(domain_id == 1)
    }

    pub fn trivially_false() -> Predicate {
        // By convention, there is a dummy 0-1 variable set to one at root.
        // We use it to denote the trivially true predicate.
        let domain_id = DomainId { id: 0 };
        predicate!(domain_id != 1)
    }
}

impl std::ops::Not for Predicate {
    type Output = Predicate;

    fn not(self) -> Self::Output {
        let domain_id = self.get_domain();
        let value = self.get_right_hand_side();

        match self.get_predicate_type() {
            PredicateType::LowerBound => predicate!(domain_id <= value - 1),
            PredicateType::UpperBound => predicate!(domain_id >= value + 1),
            PredicateType::NotEqual => predicate!(domain_id == value),
            PredicateType::Equal => predicate!(domain_id != value),
        }
    }
}

impl std::fmt::Display for Predicate {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if *self == Predicate::trivially_true() {
            write!(f, "[True]")
        } else if *self == Predicate::trivially_false() {
            write!(f, "[False]")
        } else {
            let domain_id = self.get_domain();
            let rhs = self.get_right_hand_side();

            match self.get_predicate_type() {
                PredicateType::LowerBound => write!(f, "[{domain_id} >= {rhs}]"),
                PredicateType::UpperBound => write!(f, "[{domain_id} <= {rhs}]"),
                PredicateType::NotEqual => write!(f, "[{domain_id} != {rhs}]"),
                PredicateType::Equal => write!(f, "[{domain_id} == {rhs}]"),
            }
        }
    }
}

impl std::fmt::Debug for Predicate {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }
}

#[cfg(test)]
mod test {
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
}
