use thiserror::Error;

use super::predicate::Predicate;
use crate::engine::variables::DomainId;

/// Representation of a domain operation, it can either be in the form of atomic constraints over
/// [`DomainId`]s (in the form of [`IntegerPredicate::LowerBound`],
/// [`IntegerPredicate::UpperBound`], [`IntegerPredicate::NotEqual`] or [`IntegerPredicate::Equal`])
#[derive(Clone, PartialEq, Eq, Copy, Hash)]
pub enum IntegerPredicate {
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

impl IntegerPredicate {
    pub fn is_equality_predicate(&self) -> bool {
        matches!(
            *self,
            IntegerPredicate::Equal {
                domain_id: _,
                equality_constant: _
            }
        )
    }

    pub fn is_lower_bound_predicate(&self) -> bool {
        matches!(
            *self,
            IntegerPredicate::LowerBound {
                domain_id: _,
                lower_bound: _
            }
        )
    }

    pub fn is_not_equal_predicate(&self) -> bool {
        matches!(
            *self,
            IntegerPredicate::NotEqual {
                domain_id: _,
                not_equal_constant: _
            }
        )
    }

    /// Returns the [`DomainId`] of the [`IntegerPredicate`]
    pub fn get_domain(&self) -> DomainId {
        match *self {
            IntegerPredicate::LowerBound {
                domain_id,
                lower_bound: _,
            } => domain_id,
            IntegerPredicate::UpperBound {
                domain_id,
                upper_bound: _,
            } => domain_id,
            IntegerPredicate::NotEqual {
                domain_id,
                not_equal_constant: _,
            } => domain_id,
            IntegerPredicate::Equal {
                domain_id,
                equality_constant: _,
            } => domain_id,
        }
    }
}

impl std::ops::Not for IntegerPredicate {
    type Output = IntegerPredicate;

    fn not(self) -> Self::Output {
        match self {
            IntegerPredicate::LowerBound {
                domain_id,
                lower_bound,
            } => IntegerPredicate::UpperBound {
                domain_id,
                upper_bound: lower_bound - 1,
            },
            IntegerPredicate::UpperBound {
                domain_id,
                upper_bound,
            } => IntegerPredicate::LowerBound {
                domain_id,
                lower_bound: upper_bound + 1,
            },
            IntegerPredicate::NotEqual {
                domain_id,
                not_equal_constant,
            } => IntegerPredicate::Equal {
                domain_id,
                equality_constant: not_equal_constant,
            },
            IntegerPredicate::Equal {
                domain_id,
                equality_constant,
            } => IntegerPredicate::NotEqual {
                domain_id,
                not_equal_constant: equality_constant,
            },
        }
    }
}

#[derive(Debug, Error, Copy, Clone)]
#[error("Attempt to transform non-integer predicate to integer predicate")]
pub struct IntegerPredicateConversionError;

impl TryFrom<Predicate> for IntegerPredicate {
    type Error = IntegerPredicateConversionError;

    fn try_from(value: Predicate) -> Result<Self, Self::Error> {
        match value {
            Predicate::IntegerPredicate(integer_predicate) => Ok(integer_predicate),
            _ => Err(IntegerPredicateConversionError),
        }
    }
}

impl std::fmt::Display for IntegerPredicate {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IntegerPredicate::LowerBound {
                domain_id,
                lower_bound,
            } => write!(f, "[{} >= {}]", domain_id, lower_bound),
            IntegerPredicate::UpperBound {
                domain_id,
                upper_bound,
            } => write!(f, "[{} <= {}]", domain_id, upper_bound),
            IntegerPredicate::NotEqual {
                domain_id,
                not_equal_constant,
            } => write!(f, "[{} != {}]", domain_id, not_equal_constant),
            IntegerPredicate::Equal {
                domain_id,
                equality_constant,
            } => write!(f, "[{} == {}]", domain_id, equality_constant),
        }
    }
}

impl std::fmt::Debug for IntegerPredicate {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}
