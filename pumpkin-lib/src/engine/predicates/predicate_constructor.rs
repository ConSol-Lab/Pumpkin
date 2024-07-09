use super::integer_predicate::IntegerPredicate;
use crate::engine::variables::DomainId;

pub trait PredicateConstructor {
    type Value;

    fn lower_bound_predicate(&self, bound: Self::Value) -> IntegerPredicate;
    fn upper_bound_predicate(&self, bound: Self::Value) -> IntegerPredicate;
    fn equality_predicate(&self, bound: Self::Value) -> IntegerPredicate;
    fn disequality_predicate(&self, bound: Self::Value) -> IntegerPredicate;
}

impl PredicateConstructor for DomainId {
    type Value = i32;

    fn lower_bound_predicate(&self, bound: Self::Value) -> IntegerPredicate {
        IntegerPredicate::LowerBound {
            domain_id: *self,
            lower_bound: bound,
        }
    }

    fn upper_bound_predicate(&self, bound: Self::Value) -> IntegerPredicate {
        IntegerPredicate::UpperBound {
            domain_id: *self,
            upper_bound: bound,
        }
    }

    fn equality_predicate(&self, bound: Self::Value) -> IntegerPredicate {
        IntegerPredicate::Equal {
            domain_id: *self,
            equality_constant: bound,
        }
    }

    fn disequality_predicate(&self, bound: Self::Value) -> IntegerPredicate {
        IntegerPredicate::NotEqual {
            domain_id: *self,
            not_equal_constant: bound,
        }
    }
}

#[macro_export]
macro_rules! predicate {
    ($($var:ident).+$([$index:expr])? >= $bound:expr) => {{
        #[allow(unused_imports)]
        use $crate::engine::predicates::predicate_constructor::PredicateConstructor;
        $($var).+$([$index])?.lower_bound_predicate($bound)
    }};
    ($($var:ident).+$([$index:expr])? <= $bound:expr) => {{
        #[allow(unused_imports)]
        use $crate::engine::predicates::predicate_constructor::PredicateConstructor;
        $($var).+$([$index])?.upper_bound_predicate($bound)
    }};
    ($($var:ident).+$([$index:expr])? == $value:expr) => {{
        #[allow(unused_imports)]
        use $crate::engine::predicates::predicate_constructor::PredicateConstructor;
        $($var).+$([$index])?.equality_predicate($value)
    }};
    ($($var:ident).+$([$index:expr])? != $value:expr) => {{
        #[allow(unused_imports)]
        use $crate::engine::predicates::predicate_constructor::PredicateConstructor;
        $($var).+$([$index])?.disequality_predicate($value)
    }};
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn macro_local_identifiers_are_matched() {
        let x = DomainId { id: 0 };

        let lower_bound_predicate = IntegerPredicate::LowerBound {
            domain_id: x,
            lower_bound: 2,
        };
        let upper_bound_predicate = IntegerPredicate::UpperBound {
            domain_id: x,
            upper_bound: 3,
        };
        let equality_predicate = IntegerPredicate::Equal {
            domain_id: x,
            equality_constant: 5,
        };
        let disequality_predicate = IntegerPredicate::NotEqual {
            domain_id: x,
            not_equal_constant: 5,
        };

        assert_eq!(lower_bound_predicate, predicate![x >= 2]);
        assert_eq!(upper_bound_predicate, predicate![x <= 3]);
        assert_eq!(equality_predicate, predicate![x == 5]);
        assert_eq!(disequality_predicate, predicate![x != 5]);
    }

    #[test]
    fn macro_nested_identifiers_are_matched() {
        struct Wrapper {
            x: DomainId,
        }

        let wrapper = Wrapper {
            x: DomainId { id: 0 },
        };

        let lower_bound_predicate = IntegerPredicate::LowerBound {
            domain_id: wrapper.x,
            lower_bound: 2,
        };
        assert_eq!(lower_bound_predicate, predicate![wrapper.x >= 2]);

        let upper_bound_predicate = IntegerPredicate::UpperBound {
            domain_id: wrapper.x,
            upper_bound: 3,
        };
        assert_eq!(upper_bound_predicate, predicate![wrapper.x <= 3]);

        let equality_predicate = IntegerPredicate::Equal {
            domain_id: wrapper.x,
            equality_constant: 5,
        };
        assert_eq!(equality_predicate, predicate![wrapper.x == 5]);

        let disequality_predicate = IntegerPredicate::NotEqual {
            domain_id: wrapper.x,
            not_equal_constant: 5,
        };
        assert_eq!(disequality_predicate, predicate![wrapper.x != 5]);
    }

    #[test]
    fn macro_index_expressions_are_matched() {
        let wrapper = [DomainId { id: 0 }];

        let lower_bound_predicate = IntegerPredicate::LowerBound {
            domain_id: wrapper[0],
            lower_bound: 2,
        };
        assert_eq!(lower_bound_predicate, predicate![wrapper[0] >= 2]);

        let upper_bound_predicate = IntegerPredicate::UpperBound {
            domain_id: wrapper[0],
            upper_bound: 3,
        };
        assert_eq!(upper_bound_predicate, predicate![wrapper[0] <= 3]);

        let equality_predicate = IntegerPredicate::Equal {
            domain_id: wrapper[0],
            equality_constant: 5,
        };
        assert_eq!(equality_predicate, predicate![wrapper[0] == 5]);

        let disequality_predicate = IntegerPredicate::NotEqual {
            domain_id: wrapper[0],
            not_equal_constant: 5,
        };
        assert_eq!(disequality_predicate, predicate![wrapper[0] != 5]);
    }
}
