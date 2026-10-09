use std::borrow::Cow;

use pumpkin_checking::ConflictChecker;

use crate::predicates::Predicate;
use crate::propagation::ConstraintDescription;
#[cfg(doc)]
use crate::propagation::PropagatorConstructor;

/// The inference rule that a propagator implements.
///
/// The conflict checker of a rule verifies that every propagation is an instance of the rule. It
/// is created from the description of the constraint when the rule is registered, once per
/// constraint, so the checker and the propagator work on the same data.
///
/// A propagator names the rule it implements through [`PropagatorConstructor::Rule`]. The name of
/// the rule identifies its inferences in proofs.
pub trait ConflictRule {
    type Description: ConstraintDescription + 'static;

    fn name() -> Cow<'static, str>;

    fn create_conflict_checker(
        constraint_description: &Self::Description,
    ) -> impl ConflictChecker<Predicate> + 'static;
}
