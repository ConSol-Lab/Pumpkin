use std::borrow::Cow;

use pumpkin_checking::InferenceChecker;
use pumpkin_checking::RetentionChecker;

use crate::checkers::Scope;
use crate::predicates::Predicate;

/// The data of a constraint, independent of the propagator that implements it.
///
/// The [`ConflictRule`] of the constraint is defined over its description, so the checkers of the
/// rule see the same data as the propagator.
pub trait ConstraintDescription {
    /// The variables of the constraint, whose domain changes trigger its retention checker.
    fn scope(&self) -> Scope;
}

/// The inference rule that a propagator implements.
///
/// A rule is checked in two directions: its inference checker verifies that every propagation is
/// an instance of the rule, and its retention checker verifies that nothing the rule allows is
/// left to propagate at a fixpoint. Both are created from the [`ConstraintDescription`] of the
/// constraint when the rule is registered, once per constraint.
pub trait ConflictRule {
    /// The data of the constraint that the rule is defined over.
    type Description: ConstraintDescription;

    /// The name of the rule, which identifies its inferences in proofs.
    fn name() -> Cow<'static, str>;

    /// Create the checker that verifies the inferences of the rule for the given constraint.
    fn create_inference_checker(
        description: &Self::Description,
    ) -> impl InferenceChecker<Predicate> + 'static;

    /// Create the checker that verifies that nothing is left to propagate for the given
    /// constraint.
    fn create_retention_checker(
        description: &Self::Description,
    ) -> impl RetentionChecker<Predicate> + 'static;
}
