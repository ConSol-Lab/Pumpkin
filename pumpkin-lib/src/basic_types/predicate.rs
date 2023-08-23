use super::DomainId;

#[derive(Clone, PartialEq, Eq, Copy)]
pub enum Predicate {
    LowerBound {
        integer_variable: DomainId,
        lower_bound: i32,
    },
    UpperBound {
        integer_variable: DomainId,
        upper_bound: i32,
    },
    NotEqual {
        integer_variable: DomainId,
        not_equal_constant: i32,
    },
    Equal {
        integer_variable: DomainId,
        equality_constant: i32,
    },
}

impl Predicate {
    pub fn is_equality_predicate(&self) -> bool {
        matches!(
            *self,
            Predicate::Equal {
                integer_variable: _,
                equality_constant: _
            }
        )
    }

    pub fn is_lower_bound_predicate(&self) -> bool {
        matches!(
            *self,
            Predicate::LowerBound {
                integer_variable: _,
                lower_bound: _
            }
        )
    }

    pub fn is_not_equal_predicate(&self) -> bool {
        matches!(
            *self,
            Predicate::NotEqual {
                integer_variable: _,
                not_equal_constant: _
            }
        )
    }

    pub fn get_right_hand_side(&self) -> i32 {
        match *self {
            Predicate::LowerBound {
                integer_variable: _,
                lower_bound,
            } => lower_bound,
            Predicate::UpperBound {
                integer_variable: _,
                upper_bound,
            } => upper_bound,
            Predicate::NotEqual {
                integer_variable: _,
                not_equal_constant,
            } => not_equal_constant,
            Predicate::Equal {
                integer_variable: _,
                equality_constant,
            } => equality_constant,
        }
    }

    pub fn get_integer_variable(&self) -> DomainId {
        match *self {
            Predicate::LowerBound {
                integer_variable,
                lower_bound: _,
            } => integer_variable,
            Predicate::UpperBound {
                integer_variable,
                upper_bound: _,
            } => integer_variable,
            Predicate::NotEqual {
                integer_variable,
                not_equal_constant: _,
            } => integer_variable,
            Predicate::Equal {
                integer_variable,
                equality_constant: _,
            } => integer_variable,
        }
    }

    pub fn get_dummy_predicate() -> Predicate {
        let integer_variable = DomainId { id: u32::MAX };
        Predicate::Equal {
            integer_variable,
            equality_constant: i32::MAX,
        }
    }
}

impl std::ops::Not for Predicate {
    type Output = Predicate;
    fn not(self) -> Predicate {
        match self {
            Predicate::LowerBound {
                integer_variable,
                lower_bound,
            } => Predicate::UpperBound {
                integer_variable,
                upper_bound: lower_bound - 1,
            },
            Predicate::UpperBound {
                integer_variable,
                upper_bound,
            } => Predicate::LowerBound {
                integer_variable,
                lower_bound: upper_bound + 1,
            },
            Predicate::NotEqual {
                integer_variable,
                not_equal_constant,
            } => Predicate::Equal {
                integer_variable,
                equality_constant: not_equal_constant,
            },
            Predicate::Equal {
                integer_variable,
                equality_constant,
            } => Predicate::NotEqual {
                integer_variable,
                not_equal_constant: equality_constant,
            },
        }
    }
}

impl std::fmt::Display for Predicate {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Predicate::LowerBound {
                integer_variable,
                lower_bound,
            } => write!(f, "[{} >= {}]", integer_variable, lower_bound),
            Predicate::UpperBound {
                integer_variable,
                upper_bound,
            } => write!(f, "[{} <= {}]", integer_variable, upper_bound),
            Predicate::NotEqual {
                integer_variable,
                not_equal_constant,
            } => write!(f, "[{} != {}]", integer_variable, not_equal_constant),
            Predicate::Equal {
                integer_variable,
                equality_constant,
            } => write!(f, "[{} == {}]", integer_variable, equality_constant),
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
            integer_variable: *self,
            lower_bound: bound,
        }
    }

    fn upper_bound_predicate(&self, bound: Self::Value) -> Predicate {
        Predicate::UpperBound {
            integer_variable: *self,
            upper_bound: bound,
        }
    }

    fn equality_predicate(&self, bound: Self::Value) -> Predicate {
        Predicate::Equal {
            integer_variable: *self,
            equality_constant: bound,
        }
    }

    fn disequality_predicate(&self, bound: Self::Value) -> Predicate {
        Predicate::NotEqual {
            integer_variable: *self,
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
                integer_variable: x,
                lower_bound: 2,
            },
            predicate![x >= 2]
        );
        assert_eq!(
            Predicate::UpperBound {
                integer_variable: x,
                upper_bound: 3,
            },
            predicate![x <= 3]
        );
        assert_eq!(
            Predicate::Equal {
                integer_variable: x,
                equality_constant: 5
            },
            predicate![x == 5]
        );
        assert_eq!(
            Predicate::NotEqual {
                integer_variable: x,
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
                integer_variable: wrapper.x,
                lower_bound: 2,
            },
            predicate![wrapper.x >= 2]
        );
        assert_eq!(
            Predicate::UpperBound {
                integer_variable: wrapper.x,
                upper_bound: 3,
            },
            predicate![wrapper.x <= 3]
        );
        assert_eq!(
            Predicate::Equal {
                integer_variable: wrapper.x,
                equality_constant: 5
            },
            predicate![wrapper.x == 5]
        );
        assert_eq!(
            Predicate::NotEqual {
                integer_variable: wrapper.x,
                not_equal_constant: 5,
            },
            predicate![wrapper.x != 5]
        );
    }
}
