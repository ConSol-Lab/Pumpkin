use crate::engine::variables::DomainId;
use crate::predicate;

/// Representation of a domain operation, also known as an atomic constraint. It is a triple
/// ([`DomainId`], [`PredicateType`], value).
///
/// To create a [`Predicate`], use [Predicate::new] or the more concise [predicate!] macro.
#[derive(Clone, PartialEq, Eq, Copy, Hash)]
pub struct Predicate {
    /// The two most significant bits of the id stored in the [`Predicate`] contains the type of
    /// predicate.
    id: u32,
    value: i32,
}

const LOWER_BOUND_CODE: u8 = 1;
const UPPER_BOUND_CODE: u8 = 2;
const EQUAL_CODE: u8 = 0;
const NOT_EQUAL_CODE: u8 = 3;

impl Predicate {
    /// Creates a new [`Predicate`] (also known as atomic constraint) which represents a domain
    /// operation.
    pub fn new(id: DomainId, predicate_type: PredicateType, value: i32) -> Self {
        let code = match predicate_type {
            PredicateType::LowerBound => LOWER_BOUND_CODE,
            PredicateType::UpperBound => UPPER_BOUND_CODE,
            PredicateType::NotEqual => NOT_EQUAL_CODE,
            PredicateType::Equal => EQUAL_CODE,
        };
        let id = id.id() | (code as u32) << 30;
        Self { id, value }
    }

    fn get_type_code(&self) -> u8 {
        (self.id >> 30) as u8
    }

    pub fn get_predicate_type(&self) -> PredicateType {
        (*self).into()
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Copy, Hash)]
pub enum PredicateType {
    LowerBound,
    UpperBound,
    NotEqual,
    Equal,
}
impl PredicateType {
    pub(crate) fn is_lower_bound(&self) -> bool {
        matches!(self, PredicateType::LowerBound)
    }

    pub(crate) fn is_upper_bound(&self) -> bool {
        matches!(self, PredicateType::UpperBound)
    }
}

impl From<Predicate> for PredicateType {
    fn from(value: Predicate) -> Self {
        match value.get_type_code() {
            LOWER_BOUND_CODE => Self::LowerBound,
            UPPER_BOUND_CODE => Self::UpperBound,
            EQUAL_CODE => Self::Equal,
            NOT_EQUAL_CODE => Self::NotEqual,
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
        self.get_type_code() == EQUAL_CODE
    }

    pub fn is_lower_bound_predicate(&self) -> bool {
        self.get_type_code() == LOWER_BOUND_CODE
    }

    pub fn is_upper_bound_predicate(&self) -> bool {
        self.get_type_code() == UPPER_BOUND_CODE
    }

    pub fn is_not_equal_predicate(&self) -> bool {
        self.get_type_code() == NOT_EQUAL_CODE
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
        let domain_id = DomainId::new(0);
        predicate!(domain_id == 1)
    }

    pub fn trivially_false() -> Predicate {
        // By convention, there is a dummy 0-1 variable set to one at root.
        // We use it to denote the trivially true predicate.
        let domain_id = DomainId::new(0);
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
    use crate::predicate;
    use crate::variables::DomainId;

    #[test]
    fn are_mutually_exclusive() {
        let domain_id = DomainId::new(0);

        assert!(!predicate!(domain_id >= 5).is_mutually_exclusive_with(predicate!(domain_id >= 7)));
        assert!(!predicate!(domain_id >= 5).is_mutually_exclusive_with(predicate!(domain_id != 2)));
        assert!(!predicate!(domain_id <= 5).is_mutually_exclusive_with(predicate!(domain_id <= 8)));
        assert!(!predicate!(domain_id <= 5).is_mutually_exclusive_with(predicate!(domain_id != 8)));
        assert!(!predicate!(domain_id != 9).is_mutually_exclusive_with(predicate!(domain_id >= 8)));
        assert!(!predicate!(domain_id != 9).is_mutually_exclusive_with(predicate!(domain_id <= 8)));
        assert!(!predicate!(domain_id != 9).is_mutually_exclusive_with(predicate!(domain_id != 8)));

        assert!(predicate!(domain_id <= 7).is_mutually_exclusive_with(predicate!(domain_id >= 8)));
        assert!(predicate!(domain_id >= 8).is_mutually_exclusive_with(predicate!(domain_id <= 7)));

        assert!(predicate!(domain_id >= 8).is_mutually_exclusive_with(predicate!(domain_id == 7)));
        assert!(predicate!(domain_id == 7).is_mutually_exclusive_with(predicate!(domain_id >= 8)));

        assert!(predicate!(domain_id == 7).is_mutually_exclusive_with(predicate!(domain_id <= 6)));
        assert!(predicate!(domain_id <= 6).is_mutually_exclusive_with(predicate!(domain_id == 7)));

        assert!(predicate!(domain_id != 8).is_mutually_exclusive_with(predicate!(domain_id == 8)));
        assert!(predicate!(domain_id == 8).is_mutually_exclusive_with(predicate!(domain_id != 8)));

        assert!(predicate!(domain_id == 7).is_mutually_exclusive_with(predicate!(domain_id == 8)));
    }

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
