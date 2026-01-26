use crate::{predicates::Predicate, variables::DomainId};

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
pub struct Hypercube {}

impl Hypercube {
    /// Create a new hypercube from a sequence of predicates.
    ///
    /// If the predicates are inconsistent, the [`Err`] variant is returned.
    pub fn new(
        predicates: impl IntoIterator<Item = Predicate>,
    ) -> Result<Self, InconsistentHypercube> {
        // Note: Ideally this would be an implementation of [`TryFrom`], however, that cannot be
        // done in the same way due to a 'conflicting implementations' error.
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use crate::{predicate, state::State};

    use super::*;

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
}
