use super::IntegerVariable;

#[derive(Clone, PartialEq, Eq, Copy)]
pub enum Predicate {
    LowerBound {
        integer_variable: IntegerVariable,
        lower_bound: i32,
    },
    UpperBound {
        integer_variable: IntegerVariable,
        upper_bound: i32,
    },
    NotEqual {
        integer_variable: IntegerVariable,
        not_equal_constant: i32,
    },
    Equal {
        integer_variable: IntegerVariable,
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

    pub fn get_integer_variable(&self) -> IntegerVariable {
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
        let integer_variable = IntegerVariable { id: u32::MAX };
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
