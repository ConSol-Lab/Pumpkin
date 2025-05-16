use crate::engine::variables::DomainId;
use crate::predicate;

/// Representation of a domain operation.
///
/// It can either be in the form of atomic constraints over
/// [`DomainId`]s (in the form of [`PredicateType::LowerBound`],
/// [`PredicateType::UpperBound`], [`Predicate::NotEqual`] or [`Predicate::Equal`])
#[derive(Clone, PartialEq, Eq, Copy, Hash)]
pub struct Predicate {
    id: u32,
    value: i32,
}

impl Predicate {
    pub(crate) fn new(id: u32, value: i32) -> Self {
        Self { id, value }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Copy, Hash)]
pub enum PredicateType {
    LowerBound {
        domain_id: DomainId,
        lower_bound: i32,
    },
    UpperBound {
        domain_id: DomainId,
        upper_bound: i32,
    },
    NotEqual {
        domain_id: DomainId,
        not_equal_constant: i32,
    },
    Equal {
        domain_id: DomainId,
        equality_constant: i32,
    },
}

impl From<Predicate> for PredicateType {
    fn from(value: Predicate) -> Self {
        let domain_id = value.get_domain();
        let rhs = value.get_right_hand_side();
        if value.is_lower_bound_predicate() {
            Self::LowerBound {
                domain_id,
                lower_bound: rhs,
            }
        } else if value.is_upper_bound_predicate() {
            Self::UpperBound {
                domain_id,
                upper_bound: rhs,
            }
        } else if value.is_equality_predicate() {
            Self::Equal {
                domain_id,
                equality_constant: rhs,
            }
        } else if value.is_not_equal_predicate() {
            Self::NotEqual {
                domain_id,
                not_equal_constant: rhs,
            }
        } else {
            panic!()
        }
    }
}

impl Predicate {
    pub(crate) fn get_type(&self) -> PredicateType {
        (*self).into()
    }

    pub(crate) fn is_mutually_exclusive_with(self, other: Predicate) -> bool {
        // TODO
        todo!()
    }

    pub fn is_equality_predicate(&self) -> bool {
        self.id >> 30 == 0
    }

    pub fn is_lower_bound_predicate(&self) -> bool {
        self.id >> 30 == 1
    }

    pub fn is_upper_bound_predicate(&self) -> bool {
        self.id >> 30 == 2
    }

    pub fn is_not_equal_predicate(&self) -> bool {
        self.id >> 30 == 3
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

        if self.is_lower_bound_predicate() {
            predicate!(domain_id <= value - 1)
        } else if self.is_upper_bound_predicate() {
            predicate!(domain_id >= value + 1)
        } else if self.is_equality_predicate() {
            predicate!(domain_id != value)
        } else if self.is_not_equal_predicate() {
            predicate!(domain_id == value)
        } else {
            panic!()
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
            let value = self.get_right_hand_side();
            if self.is_lower_bound_predicate() {
                write!(f, "[{domain_id} >= {value}]")
            } else if self.is_upper_bound_predicate() {
                write!(f, "[{domain_id} <= {value}]")
            } else if self.is_not_equal_predicate() {
                write!(f, "[{domain_id} != {value}]")
            } else if self.is_equality_predicate() {
                write!(f, "[{domain_id} == {value}]")
            } else {
                panic!()
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
