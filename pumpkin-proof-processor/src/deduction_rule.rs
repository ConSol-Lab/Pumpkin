//! The rules of the processor's propagators, for the checks of the solver's framework.
//!
//! The processor's deductions are nogoods, and their inferences are checked as the solver checks
//! the inferences of its nogoods. They have no retention check: the processor splits conflict
//! detection from unit propagation over separate propagators and deactivates deductions while it
//! trims, so its propagators are not meant to reach the fixpoint of the nogoods they hold.

use std::borrow::Cow;

use pumpkin_checking::AtomicConstraint;
use pumpkin_checking::ConflictCheck;
use pumpkin_checking::ConflictChecker;
use pumpkin_checking::DomainContext;
use pumpkin_checking::RetentionCheck;
use pumpkin_checking::RetentionChecker;
use pumpkin_checking::VariableState;
use pumpkin_core::checkers::CompleteSolution;
use pumpkin_core::checkers::ScopeItem;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::propagation::ConflictRule;
use pumpkin_core::propagation::ConstraintDescription;
use pumpkin_core::propagation::SolutionCheck;
use pumpkin_core::propagators::nogoods::NogoodDescription;
use pumpkin_core::propagators::nogoods::UnitNogoodRule;

/// The rule of a deduction: the conflict checker of a nogood, under the name of the solver's
/// nogood rule, and no retention check.
#[derive(Clone, Copy, Debug)]
pub(crate) struct DeductionRule;

impl ConflictRule for DeductionRule {
    type Description = NogoodDescription;

    fn name() -> Cow<'static, str> {
        UnitNogoodRule::name()
    }

    fn create_conflict_checker(
        constraint_description: &NogoodDescription,
    ) -> impl ConflictChecker<Predicate> + 'static {
        UnitNogoodRule::create_conflict_checker(constraint_description)
    }

    fn create_retention_checker(
        _: &NogoodDescription,
    ) -> impl RetentionChecker<Predicate> + 'static {
        NoRetentionCheck
    }
}

/// The description of a deduction database, which holds no constraint of its own: each
/// deduction it holds has the description and checkers of [`DeductionRule`].
#[derive(Clone, Debug, ScopeItem)]
pub(crate) struct DeductionDatabaseDescription {}

impl ConstraintDescription for DeductionDatabaseDescription {
    fn check_solution(&self, _: &CompleteSolution<'_>) -> SolutionCheck {
        SolutionCheck::ConstraintSatisfied
    }
}

/// The rule of a deduction database. A database posts every inference under the inference code
/// of the deduction that makes it, so no inference is made under this rule, and its conflict
/// checker confirms none.
#[derive(Clone, Copy, Debug)]
pub(crate) struct DeductionDatabaseRule;

impl ConflictRule for DeductionDatabaseRule {
    type Description = DeductionDatabaseDescription;

    fn name() -> Cow<'static, str> {
        Cow::Borrowed("deduction_database")
    }

    fn create_conflict_checker(
        _: &DeductionDatabaseDescription,
    ) -> impl ConflictChecker<Predicate> + 'static {
        NoConflict
    }

    fn create_retention_checker(
        _: &DeductionDatabaseDescription,
    ) -> impl RetentionChecker<Predicate> + 'static {
        NoRetentionCheck
    }
}

/// Accepts every state as a fixpoint.
#[derive(Clone, Copy, Debug)]
struct NoRetentionCheck;

impl<Atomic: AtomicConstraint> RetentionChecker<Atomic> for NoRetentionCheck {
    fn check_retention(&self, _: &dyn DomainContext<Atomic>) -> RetentionCheck {
        RetentionCheck::NothingToPropagate
    }
}

/// Confirms no conflict.
#[derive(Clone, Copy, Debug)]
struct NoConflict;

impl<Atomic: AtomicConstraint> ConflictChecker<Atomic> for NoConflict {
    fn check(&self, _: &VariableState<Atomic>, _: &[Atomic], _: Option<&Atomic>) -> ConflictCheck {
        ConflictCheck::NoConflictDetected
    }
}
