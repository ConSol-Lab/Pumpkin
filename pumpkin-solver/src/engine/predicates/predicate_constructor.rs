use super::predicate::{Atom, Comparator, Predicate};
use crate::engine::variables::DomainId;

/// A trait which defines methods for creating a [`Predicate`].
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
        Predicate::Atom(Atom {
            domain_id: *self,
            comparator: Comparator::GreaterEqual,
            value: bound,
        })
    }

    fn upper_bound_predicate(&self, bound: Self::Value) -> Predicate {
        Predicate::Atom(Atom {
            domain_id: *self,
            comparator: Comparator::LessEqual,
            value: bound,
        })
    }

    fn equality_predicate(&self, bound: Self::Value) -> Predicate {
        Predicate::Atom(Atom {
            domain_id: *self,
            comparator: Comparator::Equal,
            value: bound,
        })
    }

    fn disequality_predicate(&self, bound: Self::Value) -> Predicate {
        Predicate::Atom(Atom {
            domain_id: *self,
            comparator: Comparator::NotEqual,
            value: bound,
        })
    }
}

/// A macro which allows for the creation of a [`Predicate`].
///
/// # Example
/// ```rust
/// # use pumpkin_solver::Solver;
/// # use pumpkin_solver::predicate;
/// # use pumpkin_solver::predicates::Predicate;
/// let mut solver = Solver::default();
/// let x = solver.new_bounded_integer(0, 10);
///
/// let lower_bound_predicate = predicate!(x >= 5);
/// assert_eq!(
///     lower_bound_predicate,
///     Predicate::LowerBound {
///         domain_id: x,
///         lower_bound: 5
///     }
///     .into()
/// );
///
/// let upper_bound_predicate = predicate!(x <= 5);
/// assert_eq!(
///     upper_bound_predicate,
///     Predicate::UpperBound {
///         domain_id: x,
///         upper_bound: 5
///     }
///     .into()
/// );
///
/// let equality_predicate = predicate!(x == 5);
/// assert_eq!(
///     equality_predicate,
///     Predicate::Equal {
///         domain_id: x,
///         equality_constant: 5
///     }
///     .into()
/// );
///
/// let disequality_predicate = predicate!(x != 5);
/// assert_eq!(
///     disequality_predicate,
///     Predicate::NotEqual {
///         domain_id: x,
///         not_equal_constant: 5
///     }
///     .into()
/// );
/// ```
#[macro_export]
macro_rules! predicate {
    ($($var:ident).+$([$index:expr])? >= $bound:expr) => {{
        #[allow(unused, reason = "could be imported at call-site")]
        use $crate::predicates::PredicateConstructor;
        $($var).+$([$index])?.lower_bound_predicate($bound)
    }};
    ($($var:ident).+$([$index:expr])? <= $bound:expr) => {{
        #[allow(unused, reason = "could be imported at call-site")]
        use $crate::predicates::PredicateConstructor;
        $($var).+$([$index])?.upper_bound_predicate($bound)
    }};
    ($($var:ident).+$([$index:expr])? == $value:expr) => {{
        #[allow(unused, reason = "could be imported at call-site")]
        use $crate::predicates::PredicateConstructor;
        $($var).+$([$index])?.equality_predicate($value)
    }};
    ($($var:ident).+$([$index:expr])? != $value:expr) => {{
        #[allow(unused, reason = "could be imported at call-site")]
        use $crate::predicates::PredicateConstructor;
        $($var).+$([$index])?.disequality_predicate($value)
    }};
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn macro_local_identifiers_are_matched() {
        let domain_id = DomainId { id: 0 };

        let lower_bound_predicate = Predicate::Atom(Atom {
            domain_id,
            comparator: Comparator::GreaterEqual,
            value: 2,
        });
        let upper_bound_predicate = Predicate::Atom(Atom {
            domain_id,
            comparator: Comparator::LessEqual,
            value: 3,
        });
        let equality_predicate = Predicate::Atom(Atom {
            domain_id,
            comparator: Comparator::Equal,
            value: 5,
        });
        let disequality_predicate = Predicate::Atom(Atom {
            domain_id,
            comparator: Comparator::Equal,
            value: 5,
        });

        assert_eq!(lower_bound_predicate, predicate![domain_id >= 2]);
        assert_eq!(upper_bound_predicate, predicate![domain_id <= 3]);
        assert_eq!(equality_predicate, predicate![domain_id == 5]);
        assert_eq!(disequality_predicate, predicate![domain_id != 5]);
    }

    #[test]
    fn macro_nested_identifiers_are_matched() {
        struct Wrapper {
            x: DomainId,
        }

        let wrapper = Wrapper {
            x: DomainId { id: 0 },
        };

        let lower_bound_predicate = Predicate::Atom(Atom {
            domain_id: wrapper.x,
            comparator: Comparator::GreaterEqual,
            value: 2,
        });
        assert_eq!(lower_bound_predicate, predicate![wrapper.x >= 2]);

        let upper_bound_predicate = Predicate::Atom(Atom {
            domain_id: wrapper.x,
            comparator: Comparator::LessEqual,
            value: 3,
        });
        assert_eq!(upper_bound_predicate, predicate![wrapper.x <= 3]);

        let equality_predicate = Predicate::Atom(Atom {
            domain_id: wrapper.x,
            comparator: Comparator::Equal,
            value: 5,
        });
        assert_eq!(equality_predicate, predicate![wrapper.x == 5]);

        let disequality_predicate = Predicate::Atom(Atom {
            domain_id: wrapper.x,
            comparator: Comparator::NotEqual,
            value: 5,
        });
        assert_eq!(disequality_predicate, predicate![wrapper.x != 5]);
    }

    #[test]
    fn macro_index_expressions_are_matched() {
        let wrapper = [DomainId { id: 0 }];

        let lower_bound_predicate = Predicate::Atom(Atom {
            domain_id: wrapper[0],
            comparator: Comparator::GreaterEqual,
            value: 2,
        });
        assert_eq!(lower_bound_predicate, predicate![wrapper[0] >= 2]);

        let upper_bound_predicate = Predicate::Atom(Atom {
            domain_id: wrapper[0],
            comparator: Comparator::LessEqual,
            value: 3,
        });
        assert_eq!(upper_bound_predicate, predicate![wrapper[0] <= 3]);

        let equality_predicate = Predicate::Atom(Atom {
            domain_id: wrapper[0],
            comparator: Comparator::Equal,
            value: 5,
        });
        assert_eq!(equality_predicate, predicate![wrapper[0] == 5]);

        let disequality_predicate = Predicate::Atom(Atom {
            domain_id: wrapper[0],
            comparator: Comparator::NotEqual,
            value: 5,
        });
        assert_eq!(disequality_predicate, predicate![wrapper[0] != 5]);
    }
}
