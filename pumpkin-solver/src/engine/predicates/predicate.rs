use crate::engine::variables::DomainId;

/// Representation of a domain operation.
///
/// It can either be in the form of atomic constraints over
/// [`DomainId`]s (in the form of [`Predicate::LowerBound`],
/// [`Predicate::UpperBound`], [`Predicate::NotEqual`] or [`Predicate::Equal`])
#[derive(Clone, PartialEq, Eq, Copy, Hash)]
pub enum Predicate {
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

impl Predicate {
    pub(crate) fn is_mutually_exclusive_with(self, other: Predicate) -> bool {
        match (self, other) {
            (Predicate::LowerBound { .. }, Predicate::LowerBound { .. })
            | (Predicate::LowerBound { .. }, Predicate::NotEqual { .. })
            | (Predicate::UpperBound { .. }, Predicate::UpperBound { .. })
            | (Predicate::UpperBound { .. }, Predicate::NotEqual { .. })
            | (Predicate::NotEqual { .. }, Predicate::LowerBound { .. })
            | (Predicate::NotEqual { .. }, Predicate::UpperBound { .. })
            | (Predicate::NotEqual { .. }, Predicate::NotEqual { .. }) => false,
            (
                Predicate::LowerBound {
                    domain_id,
                    lower_bound,
                },
                Predicate::UpperBound {
                    domain_id: domain_id_other,
                    upper_bound,
                },
            )
            | (
                Predicate::UpperBound {
                    domain_id: domain_id_other,
                    upper_bound,
                },
                Predicate::LowerBound {
                    domain_id,
                    lower_bound,
                },
            ) => domain_id == domain_id_other && lower_bound > upper_bound,
            (
                Predicate::LowerBound {
                    domain_id,
                    lower_bound,
                },
                Predicate::Equal {
                    domain_id: domain_id_other,
                    equality_constant,
                },
            )
            | (
                Predicate::Equal {
                    domain_id: domain_id_other,
                    equality_constant,
                },
                Predicate::LowerBound {
                    domain_id,
                    lower_bound,
                },
            ) => domain_id == domain_id_other && lower_bound > equality_constant,
            (
                Predicate::UpperBound {
                    domain_id,
                    upper_bound,
                },
                Predicate::Equal {
                    domain_id: domain_id_other,
                    equality_constant,
                },
            )
            | (
                Predicate::Equal {
                    domain_id: domain_id_other,
                    equality_constant,
                },
                Predicate::UpperBound {
                    domain_id,
                    upper_bound,
                },
            ) => domain_id == domain_id_other && upper_bound < equality_constant,
            (
                Predicate::NotEqual {
                    domain_id,
                    not_equal_constant,
                },
                Predicate::Equal {
                    domain_id: domain_id_other,
                    equality_constant,
                },
            )
            | (
                Predicate::Equal {
                    domain_id: domain_id_other,
                    equality_constant,
                },
                Predicate::NotEqual {
                    domain_id,
                    not_equal_constant,
                },
            ) => domain_id == domain_id_other && equality_constant == not_equal_constant,
            (
                Predicate::Equal {
                    domain_id,
                    equality_constant,
                },
                Predicate::Equal {
                    domain_id: domain_id_other,
                    equality_constant: equality_constant_other,
                },
            ) => domain_id == domain_id_other && equality_constant != equality_constant_other,
        }
    }
    pub fn is_equality_predicate(&self) -> bool {
        matches!(
            *self,
            Predicate::Equal {
                domain_id: _,
                equality_constant: _
            }
        )
    }

    pub fn is_lower_bound_predicate(&self) -> bool {
        matches!(
            *self,
            Predicate::LowerBound {
                domain_id: _,
                lower_bound: _
            }
        )
    }

    pub fn is_upper_bound_predicate(&self) -> bool {
        matches!(
            *self,
            Predicate::UpperBound {
                domain_id: _,
                upper_bound: _
            }
        )
    }

    pub fn is_not_equal_predicate(&self) -> bool {
        matches!(
            *self,
            Predicate::NotEqual {
                domain_id: _,
                not_equal_constant: _
            }
        )
    }

    /// Returns the [`DomainId`] of the [`Predicate`]
    pub fn get_domain(&self) -> DomainId {
        match *self {
            Predicate::LowerBound {
                domain_id,
                lower_bound: _,
            } => domain_id,
            Predicate::UpperBound {
                domain_id,
                upper_bound: _,
            } => domain_id,
            Predicate::NotEqual {
                domain_id,
                not_equal_constant: _,
            } => domain_id,
            Predicate::Equal {
                domain_id,
                equality_constant: _,
            } => domain_id,
        }
    }

    pub fn get_right_hand_side(&self) -> i32 {
        match self {
            Predicate::LowerBound {
                domain_id: _,
                lower_bound,
            } => *lower_bound,
            Predicate::UpperBound {
                domain_id: _,
                upper_bound,
            } => *upper_bound,
            Predicate::NotEqual {
                domain_id: _,
                not_equal_constant,
            } => *not_equal_constant,
            Predicate::Equal {
                domain_id: _,
                equality_constant,
            } => *equality_constant,
        }
    }

    pub fn trivially_true() -> Predicate {
        // By convention, there is a dummy 0-1 variable set to one at root.
        // We use it to denote the trivially true predicate.
        Predicate::Equal {
            domain_id: DomainId { id: 0 },
            equality_constant: 1,
        }
    }

    pub fn trivially_false() -> Predicate {
        // By convention, there is a dummy 0-1 variable set to one at root.
        // We use it to denote the trivially true predicate.
        Predicate::NotEqual {
            domain_id: DomainId { id: 0 },
            not_equal_constant: 1,
        }
    }
}

impl std::ops::Not for Predicate {
    type Output = Predicate;

    fn not(self) -> Self::Output {
        match self {
            Predicate::LowerBound {
                domain_id,
                lower_bound,
            } => Predicate::UpperBound {
                domain_id,
                upper_bound: lower_bound - 1,
            },
            Predicate::UpperBound {
                domain_id,
                upper_bound,
            } => Predicate::LowerBound {
                domain_id,
                lower_bound: upper_bound + 1,
            },
            Predicate::NotEqual {
                domain_id,
                not_equal_constant,
            } => Predicate::Equal {
                domain_id,
                equality_constant: not_equal_constant,
            },
            Predicate::Equal {
                domain_id,
                equality_constant,
            } => Predicate::NotEqual {
                domain_id,
                not_equal_constant: equality_constant,
            },
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
            match self {
                Predicate::LowerBound {
                    domain_id,
                    lower_bound,
                } => write!(f, "[{} >= {}]", domain_id, lower_bound),
                Predicate::UpperBound {
                    domain_id,
                    upper_bound,
                } => write!(f, "[{} <= {}]", domain_id, upper_bound),
                Predicate::NotEqual {
                    domain_id,
                    not_equal_constant,
                } => write!(f, "[{} != {}]", domain_id, not_equal_constant),
                Predicate::Equal {
                    domain_id,
                    equality_constant,
                } => write!(f, "[{} == {}]", domain_id, equality_constant),
            }
        }
    }
}

impl std::fmt::Debug for Predicate {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
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
