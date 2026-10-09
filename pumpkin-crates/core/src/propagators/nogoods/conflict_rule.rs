use std::borrow::Cow;

use pumpkin_checking::ConflictChecker;
use pumpkin_checking::RetentionChecker;
use pumpkin_checking::checkers::ExtendedNogoodChecker;
use pumpkin_checking::checkers::NogoodChecker;

use super::NogoodDescription;
use crate::checkers::RemovableRuleCheckers;
use crate::predicates::Predicate;
use crate::proof::ConstraintTag;
use crate::propagation::ConflictRule;
use crate::propagation::PropagatorId;
use crate::propagators::nogoods::PropagationMode;
use crate::state::State;

/// The rule of a nogood under unit propagation.
#[derive(Clone, Copy, Debug)]
pub struct UnitNogoodRule;

impl ConflictRule for UnitNogoodRule {
    type Description = NogoodDescription;

    fn name() -> Cow<'static, str> {
        Cow::Borrowed("nogood")
    }

    fn create_conflict_checker(
        constraint_description: &NogoodDescription,
    ) -> impl ConflictChecker<Predicate> + 'static {
        NogoodChecker {
            nogood: constraint_description.nogood.clone(),
        }
    }

    fn create_retention_checker(
        constraint_description: &NogoodDescription,
    ) -> impl RetentionChecker<Predicate> + 'static {
        NogoodChecker {
            nogood: constraint_description.nogood.clone(),
        }
    }
}

/// The rule of a nogood under extended nogood propagation.
///
/// Its inferences are those of [`UnitNogoodRule`], under the same name; only its retention
/// checker is stronger, since extended propagation removes more values.
#[derive(Clone, Copy, Debug)]
pub struct ExtendedNogoodRule;

impl ConflictRule for ExtendedNogoodRule {
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
        constraint_description: &NogoodDescription,
    ) -> impl RetentionChecker<Predicate> + 'static {
        ExtendedNogoodChecker {
            nogood: constraint_description.nogood.clone(),
        }
    }
}

impl PropagationMode {
    /// Add the checkers of the rule with which the nogood propagator propagates `nogood` in this
    /// mode.
    ///
    /// The propagator removes the returned checkers when it deletes the nogood.
    pub(crate) fn add_nogood_checkers(
        self,
        state: &mut State,
        constraint_tag: ConstraintTag,
        nogood: &[Predicate],
        propagator: PropagatorId,
    ) -> RemovableRuleCheckers {
        let constraint_description = NogoodDescription {
            nogood: nogood.into(),
        };

        match self {
            PropagationMode::UnitPropagation => state
                .add_removable_rule_checkers::<UnitNogoodRule>(
                    constraint_tag,
                    constraint_description,
                    propagator,
                ),
            PropagationMode::ExtendedNogoodPropagation => state
                .add_removable_rule_checkers::<ExtendedNogoodRule>(
                    constraint_tag,
                    constraint_description,
                    propagator,
                ),
        }
    }
}
