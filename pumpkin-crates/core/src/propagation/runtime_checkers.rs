use pumpkin_checking::BoxedChecker;
use pumpkin_checking::InferenceChecker;

use crate::checkers::BoxedRetentionChecker;
use crate::checkers::RetentionChecker;
use crate::checkers::Scope;
use crate::predicates::Predicate;
use crate::proof::ConstraintTag;
use crate::proof::InferenceCode;
use crate::proof::InferenceLabel;
#[cfg(doc)]
use crate::propagation::PropagatorConstructor;

/// Holds the runtime checkers that are added by a propagator.
///
/// Used when creating a new propagator in [`PropagatorConstructor::create`]. Two kinds of checkers
/// can be added:
/// - inference checkers, which verify that the propagations are sound,
/// - retention checkers, which verify that the propagator has nothing left to propagate.
#[derive(Clone, Debug)]
pub struct RuntimeCheckers {
    inference_checkers: Vec<(InferenceCode, BoxedChecker<Predicate>)>,
    retention_checkers: Vec<(Scope, BoxedRetentionChecker)>,
}

impl RuntimeCheckers {
    /// Create a [`RuntimeCheckers`] value which we accept may be empty.
    ///
    /// This is often not what you want. If it is expected that some checkers should be added,
    /// use [`RuntimeCheckers::builder`] instead to communicate that intention.
    pub fn empty() -> RuntimeCheckers {
        RuntimeCheckers {
            inference_checkers: vec![],
            retention_checkers: vec![],
        }
    }

    /// Create a [`RuntimeCheckersBuilder`] to add runtime checkers.
    ///
    /// The [`RuntimeCheckersBuilder::build`] will panic if no inference checkers are added.
    pub fn builder() -> RuntimeCheckersBuilder {
        RuntimeCheckersBuilder {
            checkers: RuntimeCheckers::empty(),
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

    /// Add a [`RetentionChecker`] over the given scope to verify that the propagator has nothing
    /// left to propagate at fixpoints.
    pub fn add_retention_checker(
        &mut self,
        scope: impl Into<Scope>,
        checker: impl Into<BoxedRetentionChecker>,
    ) {
        self.retention_checkers.push((scope.into(), checker.into()));
    }

    /// Add a checker that is both the inference checker and the retention checker of the
    /// propagator's rule, with the retention checker over `scope`.
    pub fn add_rule<Checker>(
        &mut self,
        scope: impl Into<Scope>,
        constraint_tag: ConstraintTag,
        inference_label: impl InferenceLabel,
        checker: Checker,
    ) -> InferenceCode
    where
        Checker: InferenceChecker<Predicate> + RetentionChecker + Clone + 'static,
    {
        let inference_code =
            self.add_inference_checker(constraint_tag, inference_label, checker.clone());
        self.add_retention_checker(scope, checker);
        inference_code
    }

    /// Split the checkers into the inference checkers and the retention checkers.
    #[allow(clippy::type_complexity, reason = "the tuple mirrors the two fields")]
    pub fn into_parts(
        self,
    ) -> (
        Vec<(InferenceCode, BoxedChecker<Predicate>)>,
        Vec<(Scope, BoxedRetentionChecker)>,
    ) {
        (self.inference_checkers, self.retention_checkers)
    }
}

/// A builder for the [`RuntimeCheckers`] that ensures at least one inference checker is added.
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

    /// Add a [`RetentionChecker`] over the given scope to verify that the propagator has nothing
    /// left to propagate at fixpoints.
    pub fn add_retention_checker(
        &mut self,
        scope: impl Into<Scope>,
        checker: impl Into<BoxedRetentionChecker>,
    ) {
        self.checkers.add_retention_checker(scope, checker);
    }

    /// Add a checker that is both the inference checker and the retention checker of the
    /// propagator's rule, with the retention checker over `scope`.
    pub fn add_rule<Checker>(
        &mut self,
        scope: impl Into<Scope>,
        constraint_tag: ConstraintTag,
        inference_label: impl InferenceLabel,
        checker: Checker,
    ) -> InferenceCode
    where
        Checker: InferenceChecker<Predicate> + RetentionChecker + Clone + 'static,
    {
        self.checkers
            .add_rule(scope, constraint_tag, inference_label, checker)
    }

    /// Finish adding runtime checkers.
    ///
    /// Panics if runtime verification is enabled and no inference checkers are added. If it is
    /// expected behavior that no checkers are added, use [`RuntimeCheckers::empty`].
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
