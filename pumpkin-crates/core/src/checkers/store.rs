use pumpkin_checking::BoxedChecker;

use crate::containers::HashMap;
use crate::predicates::Predicate;
use crate::proof::InferenceCode;

/// Owns the runtime checkers that are active.
pub trait StoresCheckers {
    fn add_inference_checker(
        &mut self,
        inference_code: InferenceCode,
        checker: BoxedChecker<Predicate>,
    );
}

#[derive(Clone, Debug, Default)]
pub struct CheckerStore {
    inference_codes: HashMap<InferenceCode, Vec<BoxedChecker<Predicate>>>,
}

impl CheckerStore {
    pub fn for_inference_code(
        &self,
        inference_code: &InferenceCode,
    ) -> impl ExactSizeIterator<Item = &BoxedChecker<Predicate>> {
        self.inference_codes
            .get(inference_code)
            .map(|checkers| itertools::Either::Left(checkers.iter()))
            .unwrap_or(itertools::Either::Right(std::iter::empty()))
    }
}

impl StoresCheckers for CheckerStore {
    fn add_inference_checker(
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
