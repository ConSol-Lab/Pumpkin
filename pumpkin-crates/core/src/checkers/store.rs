//! This module facilitates runtime verification in Pumpkin. It defines common types as well as the
//! [`CheckerStore`] that owns the checkers that are active in the solver.

use pumpkin_checking::BoxedChecker;
#[cfg(doc)]
use pumpkin_checking::InferenceChecker;

use crate::containers::HashMap;
use crate::predicates::Predicate;
use crate::proof::InferenceCode;

/// Owns the runtime checkers present in the solver.
#[derive(Clone, Debug, Default)]
pub struct CheckerStore {
    inference_codes: HashMap<InferenceCode, Vec<BoxedChecker<Predicate>>>,
}

impl CheckerStore {
    /// Get the [`InferenceChecker`]s for the given inference code.
    pub fn for_inference_code(
        &self,
        inference_code: &InferenceCode,
    ) -> impl ExactSizeIterator<Item = &BoxedChecker<Predicate>> {
        self.inference_codes
            .get(inference_code)
            .map(|checkers| itertools::Either::Left(checkers.iter()))
            .unwrap_or(itertools::Either::Right(std::iter::empty()))
    }

    pub fn add_inference_checker(
        &mut self,
        inference_code: InferenceCode,
        checker: BoxedChecker<Predicate>,
    ) {
        self.inference_codes
            .entry(inference_code.clone())
            .or_default()
            .push(checker);
    }
}
