//! This module facilitates runtime verification in Pumpkin. It defines common types as well as the
//! [`CheckerStore`] that owns the checkers that are active in the solver.

use pumpkin_checking::BoxedChecker;
#[cfg(doc)]
use pumpkin_checking::ConflictChecker;

use crate::containers::HashMap;
use crate::predicates::Predicate;
use crate::proof::InferenceCode;

/// Owns the runtime checkers present in the solver.
///
/// The runtime checkers consist of:
/// - conflict checkers, which verify that propagations are sound.
#[derive(Clone, Debug, Default)]
pub struct CheckerStore {
    /// For each inference code we associate possibly many conflict checkers.
    conflict_checkers: HashMap<InferenceCode, Vec<BoxedChecker<Predicate>>>,
}

impl CheckerStore {
    /// Get the [`ConflictChecker`]s for the given inference code.
    pub fn for_inference_code(
        &self,
        inference_code: &InferenceCode,
    ) -> impl ExactSizeIterator<Item = &BoxedChecker<Predicate>> {
        self.conflict_checkers
            .get(inference_code)
            .map(|checkers| itertools::Either::Left(checkers.iter()))
            .unwrap_or(itertools::Either::Right(std::iter::empty()))
    }

    /// Add a new conflict checker for the inference code.
    ///
    /// An inference code can have multiple checkers, so if a conflict checker was already
    /// registered for the given code, this new checker is simply added to the collection.
    pub fn add_conflict_checker(
        &mut self,
        inference_code: InferenceCode,
        checker: BoxedChecker<Predicate>,
    ) {
        self.conflict_checkers
            .entry(inference_code.clone())
            .or_default()
            .push(checker);
    }
}
