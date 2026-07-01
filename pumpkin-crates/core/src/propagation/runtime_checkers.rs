use pumpkin_checking::BoxedChecker;
use pumpkin_checking::InferenceChecker;

use crate::predicates::Predicate;
use crate::proof::ConstraintTag;
use crate::proof::InferenceCode;
use crate::proof::InferenceLabel;

/// Holds the runtime checkers that are added by a propagator.
#[derive(Clone, Debug)]
pub struct RuntimeCheckers {
    inference_checkers: Vec<(InferenceCode, BoxedChecker<Predicate>)>,
}

impl RuntimeCheckers {
    /// Create a [`RuntimeCheckers`] value which we accept may be empty.
    ///
    /// This is often not what you want. If it is expected that some checkers should be added,
    /// use [`RuntimeCheckers::builder`] instead to communicate that intention.
    pub fn empty() -> RuntimeCheckers {
        RuntimeCheckers {
            inference_checkers: vec![],
        }
    }

    /// Create a [`RuntimeCheckersBuilder`] to add runtime checkers.
    ///
    /// The [`RuntimeCheckersBuilder::build`] will panic if no checkers are added.
    pub fn builder() -> RuntimeCheckersBuilder {
        RuntimeCheckersBuilder {
            checkers: RuntimeCheckers {
                inference_checkers: vec![],
            },
        }
    }

    /// Add an [`InferenceChecker`] to verify the soundness of propagations.
    pub fn add_inference_checker(
        &mut self,
        constraint_tag: ConstraintTag,
        inference_label: impl InferenceLabel,
        checker: impl InferenceChecker<Predicate> + 'static,
    ) -> InferenceCode {
        let inference_code = InferenceCode::new(constraint_tag, inference_label);

        self.inference_checkers
            .push((inference_code.clone(), BoxedChecker::new(Box::new(checker))));

        inference_code
    }
}

impl IntoIterator for RuntimeCheckers {
    type Item = (InferenceCode, BoxedChecker<Predicate>);

    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.inference_checkers.into_iter()
    }
}

/// A builder for the [`RuntimeCheckers`] that ensures at least one checker is added.
#[derive(Clone, Debug)]
pub struct RuntimeCheckersBuilder {
    checkers: RuntimeCheckers,
}

impl RuntimeCheckersBuilder {
    /// Add an [`InferenceChecker`] to verify the soundness of propagations.
    pub fn add_inference_checker(
        &mut self,
        constraint_tag: ConstraintTag,
        inference_label: impl InferenceLabel,
        checker: impl InferenceChecker<Predicate> + 'static,
    ) -> InferenceCode {
        self.checkers
            .add_inference_checker(constraint_tag, inference_label, checker)
    }

    /// Finish adding runtime checkers.
    ///
    /// Panics if runtime verification is enabled and no checkers are added. If it is expected
    /// behavior that no checkers are added, use [`RuntimeCheckers::empty`].
    pub fn build(self) -> RuntimeCheckers {
        if cfg!(feature = "check-propagations") {
            assert!(
                !self.checkers.inference_checkers.is_empty(),
                "did not register any inference checkers"
            );
        }

        self.checkers
    }
}
