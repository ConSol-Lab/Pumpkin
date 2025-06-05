use super::predicate::Predicate;
use crate::engine::predicates::predicate::LOWER_BOUND_CODE;
use crate::engine::predicates::predicate::NOT_EQUALS_CODE;
use crate::engine::predicates::predicate::UPPER_BOUND_CODE;
use crate::engine::variables::DomainId;
use crate::pumpkin_assert_moderate;

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

    fn equality_predicate(&self, bound: Self::Value) -> Predicate {
        pumpkin_assert_moderate!(self.id >> 30 == 0);
        Predicate::new(self.id, bound)
    }

    fn lower_bound_predicate(&self, bound: Self::Value) -> Predicate {
        pumpkin_assert_moderate!(self.id >> 30 == 0);
        Predicate::new(self.id | (LOWER_BOUND_CODE as u32) << 30, bound)
    }

    fn upper_bound_predicate(&self, bound: Self::Value) -> Predicate {
        pumpkin_assert_moderate!(self.id >> 30 == 0);
        Predicate::new(self.id | (UPPER_BOUND_CODE as u32) << 30, bound)
    }

    fn disequality_predicate(&self, bound: Self::Value) -> Predicate {
        pumpkin_assert_moderate!(self.id >> 30 == 0);
        Predicate::new(self.id | (NOT_EQUALS_CODE as u32) << 30, bound)
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
