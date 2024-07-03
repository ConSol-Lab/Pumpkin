use super::integer_predicate::IntegerPredicate;
use super::predicate::Predicate;
use crate::engine::variables::DomainId;

/// A trait which defines methods for creating a [`Predicate`]. It currently only specifies the
/// creation of [`Predicate`]s based on [`IntegerPredicate`]s.
pub trait PredicateConstructor {
    /// The value used to represent a bound.
    type Value;

    /// Creates a lower-bound predicate (e.g. `[x >= v]`).
    fn lower_bound_predicate(&self, bound: Self::Value) -> Predicate;

    /// Creates an upper-bound predicate (e.g. `[x <= v]`).
    fn upper_bound_predicate(&self, bound: Self::Value) -> Predicate;

    /// Creates an equality predicate (e.g. `[x == v]`).
    fn equality_predicate(&self, bound: Self::Value) -> Predicate;

    /// Creates a disequality predicate (e.g. `[x != v]`).
    fn disequality_predicate(&self, bound: Self::Value) -> Predicate;
}

impl PredicateConstructor for DomainId {
    type Value = i32;

    fn lower_bound_predicate(&self, bound: Self::Value) -> Predicate {
        IntegerPredicate::LowerBound {
            domain_id: *self,
            lower_bound: bound,
        }
        .into()
    }

    fn upper_bound_predicate(&self, bound: Self::Value) -> Predicate {
        IntegerPredicate::UpperBound {
            domain_id: *self,
            upper_bound: bound,
        }
        .into()
    }

    fn equality_predicate(&self, bound: Self::Value) -> Predicate {
        IntegerPredicate::Equal {
            domain_id: *self,
            equality_constant: bound,
        }
        .into()
    }

    fn disequality_predicate(&self, bound: Self::Value) -> Predicate {
        IntegerPredicate::NotEqual {
            domain_id: *self,
            not_equal_constant: bound,
        }
        .into()
    }
}

/// A macro which allows for the creation of an [`IntegerPredicate`].
///
/// # Example
/// ```rust
/// # use pumpkin_lib::Solver;
/// # use pumpkin_lib::predicate;
/// # use pumpkin_lib::predicates::IntegerPredicate;
/// let mut solver = Solver::default();
/// let x = solver.new_bounded_integer(0, 10);
///
/// let lower_bound_predicate = predicate!(x >= 5);
/// assert_eq!(
///     lower_bound_predicate,
///     IntegerPredicate::LowerBound {
///         domain_id: x,
///         lower_bound: 5
///     }
///     .into()
/// );
///
/// let upper_bound_predicate = predicate!(x <= 5);
/// assert_eq!(
///     upper_bound_predicate,
///     IntegerPredicate::UpperBound {
///         domain_id: x,
///         upper_bound: 5
///     }
///     .into()
/// );
///
/// let equality_predicate = predicate!(x == 5);
/// assert_eq!(
///     equality_predicate,
///     IntegerPredicate::Equal {
///         domain_id: x,
///         equality_constant: 5
///     }
///     .into()
/// );
///
/// let disequality_predicate = predicate!(x != 5);
/// assert_eq!(
///     disequality_predicate,
///     IntegerPredicate::NotEqual {
///         domain_id: x,
///         not_equal_constant: 5
///     }
///     .into()
/// );
/// ```
#[macro_export]
macro_rules! predicate {
    ($($var:ident).+$([$index:expr])? >= $bound:expr) => {{
        #[allow(unused)]
        use $crate::predicates::PredicateConstructor;
        $($var).+$([$index])?.lower_bound_predicate($bound)
    }};
    ($($var:ident).+$([$index:expr])? <= $bound:expr) => {{
        #[allow(unused)]
        use $crate::predicates::PredicateConstructor;
        $($var).+$([$index])?.upper_bound_predicate($bound)
    }};
    ($($var:ident).+$([$index:expr])? == $value:expr) => {{
        #[allow(unused)]
        use $crate::predicates::PredicateConstructor;
        $($var).+$([$index])?.equality_predicate($value)
    }};
    ($($var:ident).+$([$index:expr])? != $value:expr) => {{
        #[allow(unused)]
        use $crate::predicates::PredicateConstructor;
        $($var).+$([$index])?.disequality_predicate($value)
    }};
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn macro_local_identifiers_are_matched() {
        let x = DomainId { id: 0 };

        let lower_bound_predicate: Predicate = IntegerPredicate::LowerBound {
            domain_id: x,
            lower_bound: 2,
        }
        .into();
        let upper_bound_predicate: Predicate = IntegerPredicate::UpperBound {
            domain_id: x,
            upper_bound: 3,
        }
        .into();
        let equality_predicate: Predicate = IntegerPredicate::Equal {
            domain_id: x,
            equality_constant: 5,
        }
        .into();
        let disequality_predicate: Predicate = IntegerPredicate::NotEqual {
            domain_id: x,
            not_equal_constant: 5,
        }
        .into();

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

        let lower_bound_predicate: Predicate = IntegerPredicate::LowerBound {
            domain_id: wrapper.x,
            lower_bound: 2,
        }
        .into();
        assert_eq!(lower_bound_predicate, predicate![wrapper.x >= 2]);

        let upper_bound_predicate: Predicate = IntegerPredicate::UpperBound {
            domain_id: wrapper.x,
            upper_bound: 3,
        }
        .into();
        assert_eq!(upper_bound_predicate, predicate![wrapper.x <= 3]);

        let equality_predicate: Predicate = IntegerPredicate::Equal {
            domain_id: wrapper.x,
            equality_constant: 5,
        }
        .into();
        assert_eq!(equality_predicate, predicate![wrapper.x == 5]);

        let disequality_predicate: Predicate = IntegerPredicate::NotEqual {
            domain_id: wrapper.x,
            not_equal_constant: 5,
        }
        .into();
        assert_eq!(disequality_predicate, predicate![wrapper.x != 5]);
    }

    #[test]
    fn macro_index_expressions_are_matched() {
        let wrapper = [DomainId { id: 0 }];

        let lower_bound_predicate: Predicate = IntegerPredicate::LowerBound {
            domain_id: wrapper[0],
            lower_bound: 2,
        }
        .into();
        assert_eq!(lower_bound_predicate, predicate![wrapper[0] >= 2]);

        let upper_bound_predicate: Predicate = IntegerPredicate::UpperBound {
            domain_id: wrapper[0],
            upper_bound: 3,
        }
        .into();
        assert_eq!(upper_bound_predicate, predicate![wrapper[0] <= 3]);

        let equality_predicate: Predicate = IntegerPredicate::Equal {
            domain_id: wrapper[0],
            equality_constant: 5,
        }
        .into();
        assert_eq!(equality_predicate, predicate![wrapper[0] == 5]);

        let disequality_predicate: Predicate = IntegerPredicate::NotEqual {
            domain_id: wrapper[0],
            not_equal_constant: 5,
        }
        .into();
        assert_eq!(disequality_predicate, predicate![wrapper[0] != 5]);
    }
}
