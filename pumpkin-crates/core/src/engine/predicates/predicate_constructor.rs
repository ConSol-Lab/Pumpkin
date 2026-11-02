use super::predicate::Predicate;

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

/// A macro which allows for the creation of a [`Predicate`].
///
/// The left-hand side can be any expression whose type implements [`PredicateConstructor`] (e.g. a
/// local variable, a field access, an index expression, or a method call), and the right-hand side
/// can be any expression evaluating to the bound. The supported comparison operators are:
/// - `>=`
/// - `<=`
/// - `==`
/// - `!=`
/// - `>`
/// - `<`
///
/// Note that the left-hand side is split at the first top-level comparison operator; expressions
/// containing `<` or `>` outside of delimiters (such as a turbofish) should be wrapped in
/// parentheses.
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
///
/// let strict_lower_bound_predicate = predicate!(x > 5);
/// assert_eq!(strict_lower_bound_predicate, predicate!(x >= 6));
///
/// let strict_upper_bound_predicate = predicate!(x < 5);
/// assert_eq!(strict_upper_bound_predicate, predicate!(x <= 4));
///
/// let variables = vec![x];
/// let indexed_predicate = predicate!(variables.iter().next().unwrap() >= 5);
/// assert_eq!(indexed_predicate, predicate!(x >= 5));
/// ```
#[macro_export]
macro_rules! predicate {
    // Once a comparison operator is encountered, all tokens before it form the left-hand side.
    (@munch [$($lhs:tt)+] >= $bound:expr) => {
        $crate::predicate!(@construct [$($lhs)+] lower_bound_predicate($bound))
    };
    (@munch [$($lhs:tt)+] <= $bound:expr) => {
        $crate::predicate!(@construct [$($lhs)+] upper_bound_predicate($bound))
    };
    (@munch [$($lhs:tt)+] == $value:expr) => {
        $crate::predicate!(@construct [$($lhs)+] equality_predicate($value))
    };
    (@munch [$($lhs:tt)+] != $value:expr) => {
        $crate::predicate!(@construct [$($lhs)+] disequality_predicate($value))
    };
    (@munch [$($lhs:tt)+] > $bound:expr) => {
        $crate::predicate!(@construct [$($lhs)+] lower_bound_predicate($bound + 1))
    };
    (@munch [$($lhs:tt)+] < $bound:expr) => {
        $crate::predicate!(@construct [$($lhs)+] upper_bound_predicate($bound - 1))
    };
    // Otherwise, the next token is moved to the left-hand side.
    (@munch [$($lhs:tt)*] $next:tt $($rest:tt)*) => {
        $crate::predicate!(@munch [$($lhs)* $next] $($rest)*)
    };
    (@construct [$($lhs:tt)+] $method:ident($rhs:expr)) => {{
        #[allow(unused, reason = "could be imported at call-site")]
        use $crate::predicates::PredicateConstructor;
        ($($lhs)+).$method($rhs)
    }};
    ($($tokens:tt)+) => {
        $crate::predicate!(@munch [] $($tokens)+)
    };
}

#[cfg(test)]
mod tests {
    use crate::variables::DomainId;

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

    #[test]
    fn macro_strict_comparisons_are_converted() {
        let x = DomainId::new(0);

        assert_eq!(predicate![x >= 3], predicate![x > 2]);
        assert_eq!(predicate![x <= 1], predicate![x < 2]);
        assert_eq!(predicate![x >= 4], predicate![x > 1 + 2]);
    }

    #[test]
    fn macro_arbitrary_expressions_are_matched() {
        struct Wrapper {
            xs: Vec<DomainId>,
        }

        impl Wrapper {
            fn get(&self, index: usize) -> DomainId {
                self.xs[index]
            }
        }

        let wrapper = Wrapper {
            xs: vec![DomainId::new(0), DomainId::new(1)],
        };
        let index = 1;

        assert_eq!(
            predicate![wrapper.xs[1] >= 2],
            predicate![wrapper.get(index) >= 2]
        );
        assert_eq!(
            predicate![wrapper.xs[1] <= 2],
            predicate![wrapper.xs[index - 1 + 1] <= 2]
        );
        assert_eq!(
            predicate![wrapper.xs[1] == 2],
            predicate![*wrapper.xs.last().unwrap() == 2]
        );
        assert_eq!(
            predicate![wrapper.xs[0] != 2],
            predicate![wrapper.xs.iter().copied().next().unwrap() != 2]
        );
        assert_eq!(
            predicate![wrapper.xs[0] >= 3],
            predicate![wrapper.get(0) > index as i32 + 1]
        );
        assert_eq!(
            predicate![wrapper.xs[0] <= 1],
            predicate![wrapper.get(0) < 2]
        );
    }
}
