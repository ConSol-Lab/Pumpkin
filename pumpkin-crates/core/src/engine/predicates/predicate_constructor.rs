use super::predicate::Predicate;
use super::predicate::PredicateType;
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

    fn equality_predicate(&self, bound: Self::Value) -> Predicate {
        Predicate::new(*self, PredicateType::Equal, bound)
    }

    fn lower_bound_predicate(&self, bound: Self::Value) -> Predicate {
        Predicate::new(*self, PredicateType::LowerBound, bound)
    }

    fn upper_bound_predicate(&self, bound: Self::Value) -> Predicate {
        Predicate::new(*self, PredicateType::UpperBound, bound)
    }

    fn disequality_predicate(&self, bound: Self::Value) -> Predicate {
        Predicate::new(*self, PredicateType::NotEqual, bound)
    }
}

/// A macro which allows for the creation of a [`Predicate`].
///
/// # Example
/// ```rust
/// # use pumpkin_core::Solver;
/// # use pumpkin_core::predicate;
/// # use pumpkin_core::predicates::Predicate;
/// let mut solver = Solver::default();
/// let x = solver.new_bounded_integer(0, 10);
///
/// let lower_bound_predicate = predicate!(x >= 5);
/// assert_eq!(lower_bound_predicate.get_domain(), x);
/// assert_eq!(lower_bound_predicate.get_right_hand_side(), 5);
///
/// let upper_bound_predicate = predicate!(x <= 5);
/// assert_eq!(upper_bound_predicate.get_domain(), x);
/// assert_eq!(upper_bound_predicate.get_right_hand_side(), 5);
///
/// let equality_predicate = predicate!(x == 5);
/// assert_eq!(equality_predicate.get_domain(), x);
/// assert_eq!(equality_predicate.get_right_hand_side(), 5);
///
/// let disequality_predicate = predicate!(x != 5);
/// assert_eq!(disequality_predicate.get_domain(), x);
/// assert_eq!(disequality_predicate.get_right_hand_side(), 5);
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
        let x = DomainId::new(0);

        assert_eq!(x, predicate![x >= 2].get_domain());
        assert_eq!(x, predicate![x <= 3].get_domain());
        assert_eq!(x, predicate![x == 5].get_domain());
        assert_eq!(x, predicate![x != 5].get_domain());

        assert_eq!(2, predicate![x >= 2].get_right_hand_side());
        assert_eq!(3, predicate![x <= 3].get_right_hand_side());
        assert_eq!(5, predicate![x == 5].get_right_hand_side());
        assert_eq!(5, predicate![x != 5].get_right_hand_side());

        assert!(predicate!(x >= 2).is_lower_bound_predicate());
        assert!(!predicate!(x >= 2).is_upper_bound_predicate());
        assert!(!predicate!(x >= 2).is_equality_predicate());
        assert!(!predicate!(x >= 2).is_not_equal_predicate());

        assert!(predicate!(x <= 3).is_upper_bound_predicate());
        assert!(!predicate!(x <= 3).is_lower_bound_predicate());
        assert!(!predicate!(x <= 3).is_equality_predicate());
        assert!(!predicate!(x <= 3).is_not_equal_predicate());

        assert!(predicate!(x == 5).is_equality_predicate());
        assert!(!predicate!(x == 5).is_lower_bound_predicate());
        assert!(!predicate!(x == 5).is_upper_bound_predicate());
        assert!(!predicate!(x == 5).is_not_equal_predicate());

        assert!(predicate!(x != 5).is_not_equal_predicate());
        assert!(!predicate!(x != 5).is_lower_bound_predicate());
        assert!(!predicate!(x != 5).is_upper_bound_predicate());
        assert!(!predicate!(x != 5).is_equality_predicate());
    }

    #[test]
    fn macro_nested_identifiers_are_matched() {
        struct Wrapper {
            x: DomainId,
        }

        let wrapper = Wrapper {
            x: DomainId::new(0),
        };

        assert_eq!(wrapper.x, predicate![wrapper.x >= 2].get_domain());
        assert_eq!(wrapper.x, predicate![wrapper.x <= 3].get_domain());
        assert_eq!(wrapper.x, predicate![wrapper.x == 5].get_domain());
        assert_eq!(wrapper.x, predicate![wrapper.x != 5].get_domain());

        assert_eq!(2, predicate![wrapper.x >= 2].get_right_hand_side());
        assert_eq!(3, predicate![wrapper.x <= 3].get_right_hand_side());
        assert_eq!(5, predicate![wrapper.x == 5].get_right_hand_side());
        assert_eq!(5, predicate![wrapper.x != 5].get_right_hand_side());
    }

    #[test]
    fn macro_index_expressions_are_matched() {
        let wrapper = [DomainId::new(0)];

        assert_eq!(wrapper[0], predicate![wrapper[0] >= 2].get_domain());
        assert_eq!(wrapper[0], predicate![wrapper[0] <= 3].get_domain());
        assert_eq!(wrapper[0], predicate![wrapper[0] == 5].get_domain());
        assert_eq!(wrapper[0], predicate![wrapper[0] != 5].get_domain());

        assert_eq!(2, predicate![wrapper[0] >= 2].get_right_hand_side());
        assert_eq!(3, predicate![wrapper[0] <= 3].get_right_hand_side());
        assert_eq!(5, predicate![wrapper[0] == 5].get_right_hand_side());
        assert_eq!(5, predicate![wrapper[0] != 5].get_right_hand_side());
    }
}
