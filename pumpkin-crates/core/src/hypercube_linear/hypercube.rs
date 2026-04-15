use std::fmt::Display;
use std::hash::Hash;

use itertools::Itertools;
use pumpkin_checking::IntExt;
use pumpkin_checking::VariableState;

use crate::predicate;
use crate::predicates::Predicate;
use crate::predicates::PredicateType;
use crate::variables::DomainId;
use crate::variables::IntegerVariable;

/// Error that occurs when constructing a [`Hypercube`].
///
/// If the domain of a variable becomes empty, the hypercube is inconsistent and cannot be
/// constructed.
#[derive(Clone, Copy, Debug, thiserror::Error, PartialEq, Eq)]
#[error("domain {0} is empty in the hypercube")]
pub struct InconsistentHypercube(DomainId);

/// A region in the solution space.
///
/// The hypercube will always be consistent.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Hypercube {
    state: VariableState<Predicate>,
    predicates: Vec<Predicate>,
}

impl Hash for Hypercube {
    fn hash<H: std::hash::Hasher>(&self, hash_state: &mut H) {
        for predicate in self.iter_predicates() {
            predicate.hash(hash_state);
        }
    }
}

impl Hypercube {
    /// Creates a new [`Hypercube`] from a single predicate.
    ///
    /// Since a single predicate cannot be inconsistent, a hypercube can always be constructed.
    pub fn from_single_predicate(predicate: Predicate) -> Hypercube {
        Hypercube::new([predicate]).expect("single predicate cannot be inconsistent")
    }

    /// Create a new hypercube from a sequence of predicates.
    ///
    /// If the predicates are inconsistent, the [`Err`] variant is returned.
    pub fn new(
        predicates: impl IntoIterator<Item = Predicate>,
    ) -> Result<Self, InconsistentHypercube> {
        // Note: Ideally this would be an implementation of [`TryFrom`], however, that cannot be
        // done in the same way due to a 'conflicting implementations' error.

        Hypercube::default().with_predicates(predicates)
    }

    /// Add a predicate to the hypercube.
    ///
    /// If the new predicate causes the hypercube to be inconsistent, an error is returned.
    pub fn with_predicate(
        mut self,
        predicate: Predicate,
    ) -> Result<Hypercube, InconsistentHypercube> {
        if self.state.is_true(&predicate) {
            // The predicate is subsumed by the existing predicates in the hypercube.
            Ok(self)
        } else if self.state.apply(&predicate) {
            // We know that the new predicate is not implied by the hypercube, so it
            // must be inserted.
            let index_to_insert = self
                .predicates
                .binary_search(&predicate)
                .expect_err("the predicate does not exist in the hypercube");

            self.predicates.insert(index_to_insert, predicate);
            minimize(&mut self.predicates);

            if cfg!(feature = "hl-checks") {
                assert_eq!(self.predicates, describe_state_with_predicates(&self.state));
            }

            Ok(self)
        } else {
            Err(InconsistentHypercube(predicate.get_domain()))
        }
    }

    /// Add all predicates from the iterator to self.
    pub fn with_predicates(
        mut self,
        predicates: impl IntoIterator<Item = Predicate>,
    ) -> Result<Hypercube, InconsistentHypercube> {
        for predicate in predicates {
            self = self.with_predicate(predicate)?;
        }

        Ok(self)
    }

    /// Get all predicates that define the hypercube.
    ///
    /// Predicates are yielded in sorted order.
    pub fn iter_predicates(&self) -> impl Iterator<Item = Predicate> + '_ {
        self.predicates.iter().copied()
    }

    pub fn lower_bound(&self, term: &impl IntegerVariable) -> i32 {
        match term.induced_lower_bound(&self.state) {
            IntExt::Int(bound) => bound,
            IntExt::NegativeInf => i32::MIN,
            IntExt::PositiveInf => i32::MAX,
        }
    }
}

/// Semantically minimize the given vector of predicates.
///
/// If the conjunction of predicates is inconsistent, the behavior is unspecified.
///
/// Panics if the vector is not sorted.
fn minimize(predicates: &mut Vec<Predicate>) {
    assert!(predicates.is_sorted());

    if predicates.len() < 2 {
        // At least two elements are needed to require minimization.
        return;
    }

    // In the first pass, we tighten the lower bounds as much as possible.
    let mut read_idx = 1;
    let mut write_idx = 0;

    while read_idx < predicates.len() {
        let p1 = predicates[write_idx];
        let p2 = predicates[read_idx];

        match tighten_lower_bound(p1, p2) {
            Some(p) => {
                predicates[write_idx] = p;
                read_idx += 1;
            }
            None => {
                write_idx += 1;
                predicates[write_idx] = predicates[read_idx];
                read_idx += 1;
            }
        }
    }

    predicates.truncate(write_idx + 1);

    if predicates.len() < 2 {
        // The truncate call may have reduced the length to 1.
        return;
    }

    // In a second pass, we tighten the upper bounds and identify equality.
    let mut read_idx = 1;
    let mut write_idx = 0;

    let num_predicates = predicates.len();
    let reverse_idx = |idx: usize| num_predicates - idx - 1;

    while read_idx < predicates.len() {
        let p1 = predicates[reverse_idx(read_idx)];
        let p2 = predicates[reverse_idx(write_idx)];

        match tighten_upper_bound_and_merge_equalities(p1, p2) {
            Some(p) => {
                predicates[reverse_idx(write_idx)] = p;
                read_idx += 1;
            }
            None => {
                write_idx += 1;
                predicates[reverse_idx(write_idx)] = predicates[reverse_idx(read_idx)];
                read_idx += 1;
            }
        }
    }

    let end_of_range_to_drain = predicates.len() - write_idx - 1;

    // Shift everything to the right.
    let _ = predicates.drain(..end_of_range_to_drain);
}

fn tighten_upper_bound_and_merge_equalities(p1: Predicate, p2: Predicate) -> Option<Predicate> {
    use PredicateType::*;
    assert!(p1 <= p2);

    if p1.get_domain() != p2.get_domain() {
        return None;
    }

    let domain = p1.get_domain();
    let p1_rhs = p1.get_right_hand_side();
    let p2_rhs = p2.get_right_hand_side();

    match (p1.get_predicate_type(), p2.get_predicate_type()) {
        (NotEqual, UpperBound) => {
            if p1_rhs == p2_rhs {
                return Some(predicate![domain <= p1_rhs - 1]);
            }
            if p1_rhs > p2_rhs {
                return Some(p2);
            }
        }
        (UpperBound, UpperBound) => return Some(p1),

        (Equal, UpperBound) => return Some(p1),

        (LowerBound, Equal) => return Some(p2),

        (LowerBound, UpperBound) => {
            if p1_rhs == p2_rhs {
                return Some(predicate![domain == p1_rhs]);
            }
        }

        // Handled in forward pass.
        (LowerBound, LowerBound)
        | (LowerBound, NotEqual)
        | (Equal, NotEqual)
        | (NotEqual, NotEqual)
        | (NotEqual, Equal) => {}

        (UpperBound, LowerBound)
        | (UpperBound, NotEqual)
        | (UpperBound, Equal)
        | (NotEqual, LowerBound)
        | (Equal, LowerBound)
        | (Equal, Equal) => unreachable!("predicate order: p1 = {p1}, p2 = {p2}"),
    }

    None
}

fn tighten_lower_bound(p1: Predicate, p2: Predicate) -> Option<Predicate> {
    use PredicateType::*;

    assert!(p1 <= p2);

    if p1.get_domain() != p2.get_domain() {
        return None;
    }

    let domain = p1.get_domain();
    let p1_rhs = p1.get_right_hand_side();
    let p2_rhs = p2.get_right_hand_side();

    match (p1.get_predicate_type(), p2.get_predicate_type()) {
        (LowerBound, LowerBound) => {
            return Some(p2);
        }
        (LowerBound, NotEqual) => {
            if p1_rhs > p2_rhs {
                // E.g. [x >= 5] & [x != 4] -> [x >= 5]
                return Some(p1);
            }

            if p1_rhs == p2_rhs {
                // E.g. [x >= 5] & [x != 5] -> [x >= 6]
                return Some(predicate![domain >= p1_rhs + 1]);
            }
        }
        (LowerBound, UpperBound) => {
            if p1_rhs == p2_rhs {
                return Some(predicate![domain == p1_rhs]);
            }
        }
        (LowerBound, Equal) => {
            assert!(p1_rhs <= p2_rhs);
            return Some(p2);
        }

        (NotEqual, Equal) => return Some(p2),
        (Equal, NotEqual) => return Some(p1),

        // Handled in backward pass.
        (NotEqual, UpperBound)
        | (UpperBound, UpperBound)
        | (Equal, UpperBound)
        | (NotEqual, NotEqual) => {}

        (UpperBound, LowerBound)
        | (UpperBound, NotEqual)
        | (UpperBound, Equal)
        | (NotEqual, LowerBound)
        | (Equal, LowerBound)
        | (Equal, Equal) => unreachable!("predicate order: p1 = {p1}, p2 = {p2}"),
    }

    None
}

impl Display for Hypercube {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.iter_predicates().format(" & "))
    }
}

/// Get a vector of predicates that describe the given state.
fn describe_state_with_predicates(state: &VariableState<Predicate>) -> Vec<Predicate> {
    let mut predicates = state
        .domains()
        .flat_map(|domain_id| {
            if let Some(value) = state.fixed_value(domain_id) {
                itertools::Either::Left(std::iter::once(predicate![domain_id == value]))
            } else {
                let lower_bound_predicate =
                    if let IntExt::Int(lower_bound) = state.lower_bound(domain_id) {
                        Some(predicate![domain_id >= lower_bound])
                    } else {
                        None
                    };
                let upper_bound_predicate =
                    if let IntExt::Int(upper_bound) = state.upper_bound(domain_id) {
                        Some(predicate![domain_id <= upper_bound])
                    } else {
                        None
                    };

                itertools::Either::Right(
                    lower_bound_predicate
                        .into_iter()
                        .chain(
                            state
                                .holes(domain_id)
                                .map(|value| predicate![domain_id != value]),
                        )
                        .chain(upper_bound_predicate),
                )
            }
        })
        .collect::<Vec<_>>();

    predicates.sort();

    predicates
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::containers::HashSet;
    use crate::state::State;
    use crate::{conjunction, predicate};

    #[test]
    fn consistent_hypercube_can_be_created() {
        let mut state = State::default();

        let x = state.new_interval_variable(1, 10, Some("x".into()));
        let y = state.new_interval_variable(1, 10, Some("y".into()));

        let maybe_hypercube = Hypercube::new(conjunction!([x >= 2] & [y >= 2]));

        assert!(maybe_hypercube.is_ok());
    }

    #[test]
    fn inconsistent_hypercube_can_be_created() {
        let mut state = State::default();

        let x = state.new_interval_variable(1, 10, Some("x".into()));
        let y = state.new_interval_variable(1, 10, Some("y".into()));

        let error = Hypercube::new(conjunction!([x >= 2] & [y >= 2] & [x <= 1]))
            .expect_err("hypercube is inconsistent");

        assert_eq!(InconsistentHypercube(x), error);
    }

    #[test]
    fn hypercube_iters_predicates_from_constructor() {
        let mut state = State::default();

        let x = state.new_interval_variable(1, 10, Some("x".into()));
        let y = state.new_interval_variable(1, 10, Some("y".into()));

        let hypercube =
            Hypercube::new(conjunction!([x >= 2] & [y >= 2])).expect("not inconsistent");

        assert_eq!(
            [predicate![x >= 2], predicate![y >= 2]]
                .into_iter()
                .collect::<HashSet<_>>(),
            hypercube.iter_predicates().collect::<HashSet<_>>(),
        );
    }

    #[test]
    fn iterating_predicates_ignores_subsumed_predicates() {
        let mut state = State::default();

        let x = state.new_interval_variable(1, 10, Some("x".into()));

        let hypercube =
            Hypercube::new(conjunction!([x >= 2] & [x >= 4])).expect("not inconsistent");

        assert_eq!(
            [predicate![x >= 4]].into_iter().collect::<HashSet<_>>(),
            hypercube.iter_predicates().collect::<HashSet<_>>(),
        );
    }

    #[test]
    fn iterated_predicates_are_returned_in_sorted_order() {
        let mut state = State::default();

        let x = state.new_interval_variable(1, 10, Some("x".into()));
        let y = state.new_interval_variable(1, 10, Some("y".into()));

        let hypercube =
            Hypercube::new(conjunction!([y >= 2] & [x <= 6] & [x >= 4])).expect("not inconsistent");

        assert_eq!(
            vec![predicate![x >= 4], predicate![x <= 6], predicate![y >= 2]],
            hypercube.iter_predicates().collect::<Vec<_>>(),
        );
    }

    #[test]
    fn strengthening_the_hypercube_does_not_introduce_duplicate_predicates() {
        let mut state = State::default();

        let x = state.new_interval_variable(1, 10, Some("x".into()));

        let hypercube = Hypercube::from_single_predicate(predicate![x >= 4])
            .with_predicate(predicate![x >= 6])
            .expect("not inconsistent");

        assert_eq!(
            vec![predicate![x >= 6]],
            hypercube.iter_predicates().collect::<Vec<_>>(),
        );
    }

    #[test]
    fn adding_equality_removes_everything_else() {
        let mut state = State::default();

        let x = state.new_interval_variable(1, 10, Some("x".into()));

        let hypercube = Hypercube::new(conjunction!([x >= 4] & [x != 5] & [x <= 7]))
            .expect("not inconsistent")
            .with_predicate(predicate![x == 6])
            .expect("not inconsistent");

        assert_eq!(
            vec![predicate![x == 6]],
            hypercube.iter_predicates().collect::<Vec<_>>(),
        );
    }

    #[test]
    fn lower_bound_and_not_equal_imply_tighter_lower_bound() {
        let mut state = State::default();

        let x = state.new_interval_variable(1, 10, Some("x".into()));

        let hypercube = Hypercube::from_single_predicate(predicate![x >= 4])
            .with_predicate(predicate![x != 4])
            .expect("not inconsistent");

        assert_eq!(
            vec![predicate![x >= 5]],
            hypercube.iter_predicates().collect::<Vec<_>>(),
        );
    }

    #[test]
    fn upper_bound_and_not_equal_imply_tighter_upper_bound() {
        let mut state = State::default();

        let x = state.new_interval_variable(1, 10, Some("x".into()));

        let hypercube = Hypercube::from_single_predicate(predicate![x <= 4])
            .with_predicate(predicate![x != 4])
            .expect("not inconsistent");

        assert_eq!(
            vec![predicate![x <= 3]],
            hypercube.iter_predicates().collect::<Vec<_>>(),
        );
    }

    #[test]
    fn new_hole_may_tighten_bound_a_lot() {
        let mut state = State::default();

        let x = state.new_interval_variable(1, 10, Some("x".into()));

        let hypercube = Hypercube::new(conjunction!([x >= 4] & [x != 5]))
            .expect("not inconsistent")
            .with_predicate(predicate![x != 4])
            .expect("not inconsistent");

        assert_eq!(
            vec![predicate![x >= 6]],
            hypercube.iter_predicates().collect::<Vec<_>>(),
        );
    }

    #[test]
    fn new_hole_can_imply_equality() {
        let mut state = State::default();

        let x = state.new_interval_variable(1, 10, Some("x".into()));

        let hypercube = Hypercube::new(conjunction!([x >= 4] & [x <= 5]))
            .expect("not inconsistent")
            .with_predicate(predicate![x != 4])
            .expect("not inconsistent");

        assert_eq!(
            vec![predicate![x == 5]],
            hypercube.iter_predicates().collect::<Vec<_>>(),
        );
    }

    #[test]
    fn different_domains_are_not_combined_during_minimization() {
        let mut state = State::default();

        let x = state.new_interval_variable(1, 10, Some("x".into()));
        let y = state.new_interval_variable(1, 10, Some("y".into()));

        let hypercube =
            Hypercube::new(conjunction!([x == 10] & [y == 10])).expect("not inconsistent");

        assert_eq!(
            vec![predicate![x == 10], predicate![y == 10]],
            hypercube.iter_predicates().collect::<Vec<_>>(),
        );
    }

    #[test]
    fn lower_and_upper_bound_are_merged_if_hole_tightens_upper() {
        let mut state = State::default();

        let x = state.new_interval_variable(1, 10, Some("x".into()));

        let hypercube = Hypercube::new(conjunction!([x >= 9] & [x != 10]))
            .expect("not inconsistent")
            .with_predicate(predicate![x <= 10])
            .expect("not inconsistent");

        assert_eq!(
            vec![predicate![x == 9]],
            hypercube.iter_predicates().collect::<Vec<_>>(),
        );
    }

    #[test]
    fn upper_bound_is_tightened() {
        let mut state = State::default();

        let x = state.new_interval_variable(1, 10, Some("x".into()));

        let hypercube = Hypercube::from_single_predicate(predicate![x <= 0])
            .with_predicate(predicate![x <= -2])
            .expect("not inconsistent");

        assert_eq!(
            vec![predicate![x <= -2]],
            hypercube.iter_predicates().collect::<Vec<_>>(),
        );
    }
}
