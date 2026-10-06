use std::borrow::Cow;

use pumpkin_checking::InferenceChecker;
use pumpkin_checking::RetentionChecker;

use crate::predicates::Predicate;
use crate::propagation::ConstraintDescription;
#[cfg(doc)]
use crate::propagation::PropagatorConstructor;

/// The inference rule that a propagator implements.
///
/// A rule is checked in two directions: its inference checker verifies that every propagation is
/// an instance of the rule, and its retention checker verifies that nothing the rule allows is
/// left to propagate at a fixpoint. Both are created from the description of the constraint when
/// the rule is registered, once per constraint.
///
/// A propagator names the rule it implements through [`PropagatorConstructor::Rule`]. The name of
/// the rule identifies its inferences in proofs.
pub trait ConflictRule {
    type Description: ConstraintDescription;

    fn name() -> Cow<'static, str>;

    fn create_inference_checker(
        description: &Self::Description,
    ) -> impl InferenceChecker<Predicate> + 'static;

    fn create_retention_checker(
        description: &Self::Description,
    ) -> impl RetentionChecker<Predicate> + 'static;
}
