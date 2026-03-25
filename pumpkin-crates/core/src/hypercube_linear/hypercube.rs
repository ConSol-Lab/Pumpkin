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
#[derive(Clone, Debug)]
pub struct Hypercube {
    state: VariableState<Predicate>,
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

        Ok(Hypercube { state })
    }

    /// Get all predicates that define the hypercube.
    pub fn iter_predicates(&self) -> impl Iterator<Item = Predicate> + '_ {
        self.state.domains().flat_map(|domain_id| {
            let lower_bound_predicate =
                if let IntExt::Int(lower_bound) = self.state.lower_bound(domain_id) {
                    Some(predicate![domain_id >= lower_bound])
                } else {
                    None
                };
            let upper_bound_predicate =
                if let IntExt::Int(upper_bound) = self.state.upper_bound(domain_id) {
                    Some(predicate![domain_id <= upper_bound])
                } else {
                    None
                };

            [lower_bound_predicate, upper_bound_predicate]
                .into_iter()
                .flatten()
                .chain(
                    self.state
                        .holes(domain_id)
                        .map(|value| predicate![domain_id != value]),
                )
        })
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
}
