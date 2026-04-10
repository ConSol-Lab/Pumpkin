use std::hash::Hash;

use pumpkin_checking::IntExt;
use pumpkin_checking::VariableState;

use crate::predicate;
use crate::predicates::Predicate;
use crate::variables::DomainId;

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
    /// Create a new hypercube from a sequence of predicates.
    ///
    /// If the predicates are inconsistent, the [`Err`] variant is returned.
    pub fn new(
        predicates: impl IntoIterator<Item = Predicate>,
    ) -> Result<Self, InconsistentHypercube> {
        // Note: Ideally this would be an implementation of [`TryFrom`], however, that cannot be
        // done in the same way due to a 'conflicting implementations' error.

        let state = VariableState::prepare_for_conflict_check(predicates, None)
            .map_err(InconsistentHypercube)?;

        // Get the description of the state in terms of predicates.
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

        Ok(Hypercube { state, predicates })
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
            let insertion_index = self.predicates.binary_search(&predicate).expect_err(
                "since the predicate is not true in the state, it does not exist in predicates",
            );

            self.predicates.insert(insertion_index, predicate);

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
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::containers::HashSet;
    use crate::predicate;
    use crate::state::State;

    #[test]
    fn consistent_hypercube_can_be_created() {
        let mut state = State::default();

        let x = state.new_interval_variable(1, 10, Some("x".into()));
        let y = state.new_interval_variable(1, 10, Some("y".into()));

        let maybe_hypercube = Hypercube::new([predicate![x >= 2], predicate![y >= 2]]);

        assert!(maybe_hypercube.is_ok());
    }

    #[test]
    fn inconsistent_hypercube_can_be_created() {
        let mut state = State::default();

        let x = state.new_interval_variable(1, 10, Some("x".into()));
        let y = state.new_interval_variable(1, 10, Some("y".into()));

        let error = Hypercube::new([predicate![x >= 2], predicate![y >= 2], predicate![x <= 1]])
            .expect_err("hypercube is inconsistent");

        assert_eq!(InconsistentHypercube(x), error);
    }

    #[test]
    fn hypercube_iters_predicates_from_constructor() {
        let mut state = State::default();

        let x = state.new_interval_variable(1, 10, Some("x".into()));
        let y = state.new_interval_variable(1, 10, Some("y".into()));

        let hypercube =
            Hypercube::new([predicate![x >= 2], predicate![y >= 2]]).expect("not inconsistent");

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
            Hypercube::new([predicate![x >= 2], predicate![x >= 4]]).expect("not inconsistent");

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
            Hypercube::new([predicate![y >= 2], predicate![x <= 6], predicate![x >= 4]])
                .expect("not inconsistent");

        assert_eq!(
            vec![predicate![x >= 4], predicate![x <= 6], predicate![y >= 2]],
            hypercube.iter_predicates().collect::<Vec<_>>(),
        );
    }
}
