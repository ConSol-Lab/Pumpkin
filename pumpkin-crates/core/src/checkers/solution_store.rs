use std::fmt::Debug;
use std::fmt::Formatter;
use std::rc::Rc;

use crate::checkers::CompleteSolution;
use crate::containers::KeyedVec;
use crate::containers::StorageKey;
use crate::proof::InferenceCode;
use crate::propagation::ConstraintDescription;
use crate::propagation::PropagatorId;
use crate::propagation::SolutionCheck;

/// Holds the descriptions of the constraints in the solver, to check that a solution satisfies
/// every one of them.
#[derive(Clone, Default)]
pub struct SolutionCheckerStore {
    /// `None` for a constraint that was removed.
    entries: KeyedVec<SolutionCheckerId, Option<Entry>>,
}

#[derive(Clone)]
struct Entry {
    constraint_description: Rc<dyn ConstraintDescription>,
    /// The propagator that propagates the constraint.
    propagator: PropagatorId,
    inference_code: InferenceCode,
}

/// The constraint that a solution does not satisfy.
#[derive(Clone, Copy, Debug)]
pub struct SolutionFailure {
    /// The propagator that propagates the constraint.
    pub propagator: PropagatorId,
    pub inference_code: InferenceCode,
}

impl SolutionCheckerStore {
    /// Add the constraint with `constraint_description`, propagated by `propagator`.
    ///
    /// Returns the identifier through which [`SolutionCheckerStore::remove`] removes it.
    pub fn add(
        &mut self,
        constraint_description: Rc<dyn ConstraintDescription>,
        propagator: PropagatorId,
        inference_code: InferenceCode,
    ) -> SolutionCheckerId {
        self.entries.push(Some(Entry {
            constraint_description,
            propagator,
            inference_code,
        }))
    }

    /// Remove the constraint with the given identifier, which no longer exists.
    pub fn remove(&mut self, constraint: SolutionCheckerId) {
        self.entries[constraint] = None;
    }

    /// Check that `solution` satisfies every constraint, stopping at the first that it violates.
    pub fn check(&self, solution: &CompleteSolution<'_>) -> Result<(), SolutionFailure> {
        for entry in self.entries.iter().flatten() {
            match entry.constraint_description.check_solution(solution) {
                SolutionCheck::ConstraintSatisfied => {}
                SolutionCheck::ConstraintViolated => {
                    return Err(SolutionFailure {
                        propagator: entry.propagator,
                        inference_code: entry.inference_code,
                    });
                }
            }
        }

        Ok(())
    }
}

impl Debug for SolutionCheckerStore {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SolutionCheckerStore")
            .field("num_entries", &self.entries.len())
            .finish()
    }
}

/// Identifies a constraint in the [`SolutionCheckerStore`], so that it can be removed.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct SolutionCheckerId(u32);

impl StorageKey for SolutionCheckerId {
    fn index(&self) -> usize {
        self.0 as usize
    }

    fn create_from_index(index: usize) -> Self {
        SolutionCheckerId(index as u32)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::predicate;
    use crate::proof::ConstraintTag;
    use crate::propagators::nogoods::NogoodDescription;
    use crate::state::State;

    #[test]
    fn the_first_violated_constraint_is_reported() {
        let mut state = State::default();
        let x = state.new_interval_variable(0, 10, None);
        let _ = state
            .post(predicate![x == 3])
            .expect("the value is in the domain");
        let solution = state.complete_solution().expect("every variable is fixed");

        let inference_code = InferenceCode::new(
            ConstraintTag::create_from_index(0),
            crate::proof::InferenceRules::default().id("test_rule"),
        );
        let violated_by = PropagatorId::create_from_index(1);
        let mut store = SolutionCheckerStore::default();
        let _ = store.add(
            Rc::new(NogoodDescription {
                nogood: Box::from([predicate![x == 4]]),
            }),
            PropagatorId::create_from_index(0),
            inference_code,
        );
        assert!(store.check(&solution).is_ok());

        let _ = store.add(
            Rc::new(NogoodDescription {
                nogood: Box::from([predicate![x == 3]]),
            }),
            violated_by,
            inference_code,
        );
        let failure = store.check(&solution).expect_err("x == 3 is excluded");
        assert_eq!(failure.propagator, violated_by);
    }
}
