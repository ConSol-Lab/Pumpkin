use std::fmt::Debug;
use std::fmt::Formatter;
use std::rc::Rc;

use pumpkin_checking::DomainView;

use crate::containers::KeyedVec;
use crate::containers::StorageKey;
use crate::predicates::Predicate;
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
    /// Either [`SolutionCheck::ConstraintViolated`] or [`SolutionCheck::UnfixedVariable`].
    pub outcome: SolutionCheck,
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

    /// Check that `domains` satisfy every constraint, stopping at the first that they do not.
    pub fn check(&self, domains: &dyn DomainView<Predicate>) -> Result<(), SolutionFailure> {
        for entry in self.entries.iter().flatten() {
            match entry.constraint_description.check_solution(domains) {
                SolutionCheck::ConstraintSatisfied => {}
                outcome @ (SolutionCheck::ConstraintViolated | SolutionCheck::UnfixedVariable) => {
                    return Err(SolutionFailure {
                        propagator: entry.propagator,
                        inference_code: entry.inference_code,
                        outcome,
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
