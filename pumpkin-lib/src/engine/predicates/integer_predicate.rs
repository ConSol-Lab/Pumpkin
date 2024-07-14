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

    pub fn is_upper_bound_predicate(&self) -> bool {
        matches!(
            *self,
            IntegerPredicate::UpperBound {
                domain_id: _,
                upper_bound: _
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

    pub fn get_right_hand_side(&self) -> i32 {
        match self {
            IntegerPredicate::LowerBound {
                domain_id: _,
                lower_bound,
            } => *lower_bound,
            IntegerPredicate::UpperBound {
                domain_id: _,
                upper_bound,
            } => *upper_bound,
            IntegerPredicate::NotEqual {
                domain_id: _,
                not_equal_constant,
            } => *not_equal_constant,
            IntegerPredicate::Equal {
                domain_id: _,
                equality_constant,
            } => *equality_constant,
        }
    }

    pub fn trivially_true() -> IntegerPredicate {
        // By convention, there is a dummy 0-1 variable set to one at root.
        // We use it to denote the trivially true predicate.
        IntegerPredicate::Equal {
            domain_id: DomainId { id: 0 },
            equality_constant: 1,
        }
    }

    pub fn trivially_false() -> IntegerPredicate {
        // By convention, there is a dummy 0-1 variable set to one at root.
        // We use it to denote the trivially true predicate.
        IntegerPredicate::Equal {
            domain_id: DomainId { id: 0 },
            equality_constant: 0,
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

impl std::fmt::Display for IntegerPredicate {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if *self == IntegerPredicate::trivially_true() {
            write!(f, "[True]")
        } else if *self == IntegerPredicate::trivially_false() {
            write!(f, "[False]")
        } else {
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
}

impl std::fmt::Debug for IntegerPredicate {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}
