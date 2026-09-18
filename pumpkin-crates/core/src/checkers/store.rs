//! This module facilitates runtime verification in Pumpkin. It defines common types as well as the
//! [`CheckerStore`] that owns the checkers that are active in the solver.

use pumpkin_checking::BoxedChecker;
#[cfg(doc)]
use pumpkin_checking::InferenceChecker;

use crate::checkers::PropagationChecker;
use crate::containers::HashMap;
use crate::predicates::Predicate;
use crate::proof::InferenceCode;

/// Owns the runtime checkers present in the solver.
///
/// The runtime checkers consist of:
/// - inference checkers, which verify that propagations are sound. Each is wrapped in a
///   [`PropagationChecker`], which evaluates the inference against the solver state.
///
/// The consistency checkers, which verify that propagation is complete, are owned by the
/// [`ConsistencyCheckerStore`](crate::checkers::ConsistencyCheckerStore) since they are scheduled
/// rather than looked up.
#[derive(Clone, Debug, Default)]
pub struct CheckerStore {
    /// For each inference code we associate possibly many inference checkers.
    inference_checkers: HashMap<InferenceCode, Vec<PropagationChecker>>,
}

impl CheckerStore {
    /// Get the [`PropagationChecker`]s for the given inference code.
    pub fn for_inference_code(
        &self,
        inference_code: &InferenceCode,
    ) -> impl ExactSizeIterator<Item = &PropagationChecker> {
        self.inference_checkers
            .get(inference_code)
            .map(|checkers| itertools::Either::Left(checkers.iter()))
            .unwrap_or(itertools::Either::Right(std::iter::empty()))
    }

    /// Add a new inference checker for the inference code.
    ///
    /// An inference code can have multiple checkers, so if an [`InferenceChecker`] was already
    /// registered for the given code, this new checker is simply added to the collection.
    pub fn add_inference_checker(
        &mut self,
        inference_code: InferenceCode,
        checker: BoxedChecker<Predicate>,
    ) {
        self.inference_checkers
            .entry(inference_code)
            .or_default()
            .push(PropagationChecker::new(checker));
    }
}
