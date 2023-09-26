use super::DomainId;

#[derive(Clone, PartialEq, Eq, Copy)]
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

    pub fn is_not_equal_predicate(&self) -> bool {
        matches!(
            *self,
            Predicate::NotEqual {
                domain_id: _,
                not_equal_constant: _
            }
        )
    }

    pub fn get_right_hand_side(&self) -> i32 {
        match *self {
            Predicate::LowerBound {
                domain_id: _,
                lower_bound,
            } => lower_bound,
            Predicate::UpperBound {
                domain_id: _,
                upper_bound,
            } => upper_bound,
            Predicate::NotEqual {
                domain_id: _,
                not_equal_constant,
            } => not_equal_constant,
            Predicate::Equal {
                domain_id: _,
                equality_constant,
            } => equality_constant,
        }
    }

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

    pub fn get_dummy_predicate() -> Predicate {
        let domain_id = DomainId { id: u32::MAX };
        Predicate::Equal {
            domain_id,
            equality_constant: i32::MAX,
        }
    }
}

impl std::ops::Not for Predicate {
    type Output = Predicate;
    fn not(self) -> Predicate {
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

impl std::fmt::Debug for Predicate {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

pub trait PredicateConstructor {
    type Value;

    fn lower_bound_predicate(&self, bound: Self::Value) -> Predicate;
    fn upper_bound_predicate(&self, bound: Self::Value) -> Predicate;
    fn equality_predicate(&self, bound: Self::Value) -> Predicate;
    fn disequality_predicate(&self, bound: Self::Value) -> Predicate;
}

impl PredicateConstructor for DomainId {
    type Value = i32;

    fn lower_bound_predicate(&self, bound: Self::Value) -> Predicate {
        Predicate::LowerBound {
            domain_id: *self,
            lower_bound: bound,
        }
    }

    fn upper_bound_predicate(&self, bound: Self::Value) -> Predicate {
        Predicate::UpperBound {
            domain_id: *self,
            upper_bound: bound,
        }
    }

    fn equality_predicate(&self, bound: Self::Value) -> Predicate {
        Predicate::Equal {
            domain_id: *self,
            equality_constant: bound,
        }
    }

    fn disequality_predicate(&self, bound: Self::Value) -> Predicate {
        Predicate::NotEqual {
            domain_id: *self,
            not_equal_constant: bound,
        }
    }
}

#[macro_export]
macro_rules! predicate {
    ($($var:ident).+ >= $bound:expr) => {{
        use $crate::basic_types::PredicateConstructor;
        $($var).+.lower_bound_predicate($bound)
    }};
    ($($var:ident).+ <= $bound:expr) => {{
        use $crate::basic_types::PredicateConstructor;
        $($var).+.upper_bound_predicate($bound)
    }};
    ($($var:ident).+ == $value:expr) => {{
        use $crate::basic_types::PredicateConstructor;
        $($var).+.equality_predicate($value)
    }};
    ($($var:ident).+ != $value:expr) => {{
        use $crate::basic_types::PredicateConstructor;
        $($var).+.disequality_predicate($value)
    }};
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn macro_local_identifiers_are_matched() {
        let x = DomainId { id: 0 };

        assert_eq!(
            Predicate::LowerBound {
                domain_id: x,
                lower_bound: 2,
            },
            predicate![x >= 2]
        );
        assert_eq!(
            Predicate::UpperBound {
                domain_id: x,
                upper_bound: 3,
            },
            predicate![x <= 3]
        );
        assert_eq!(
            Predicate::Equal {
                domain_id: x,
                equality_constant: 5
            },
            predicate![x == 5]
        );
        assert_eq!(
            Predicate::NotEqual {
                domain_id: x,
                not_equal_constant: 5,
            },
            predicate![x != 5]
        );
    }

    #[test]
    fn macro_nested_identifiers_are_matched() {
        struct Wrapper {
            x: DomainId,
        }

        let wrapper = Wrapper {
            x: DomainId { id: 0 },
        };

        assert_eq!(
            Predicate::LowerBound {
                domain_id: wrapper.x,
                lower_bound: 2,
            },
            predicate![wrapper.x >= 2]
        );
        assert_eq!(
            Predicate::UpperBound {
                domain_id: wrapper.x,
                upper_bound: 3,
            },
            predicate![wrapper.x <= 3]
        );
        assert_eq!(
            Predicate::Equal {
                domain_id: wrapper.x,
                equality_constant: 5
            },
            predicate![wrapper.x == 5]
        );
        assert_eq!(
            Predicate::NotEqual {
                domain_id: wrapper.x,
                not_equal_constant: 5,
            },
            predicate![wrapper.x != 5]
        );
    }
}
