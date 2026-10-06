use std::borrow::Cow;

use pumpkin_checking::InferenceChecker;
use pumpkin_checking::RetentionChecker;
use pumpkin_checking::checkers::ExtendedNogoodChecker;
use pumpkin_checking::checkers::NogoodChecker;

use super::NogoodDescription;
use crate::checkers::RetentionCheckerId;
use crate::predicates::Predicate;
use crate::proof::ConstraintTag;
use crate::proof::InferenceCode;
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

    fn create_inference_checker(
        description: &NogoodDescription,
    ) -> impl InferenceChecker<Predicate> + 'static {
        NogoodChecker {
            nogood: description.nogood.clone(),
        }
    }

    fn create_retention_checker(
        description: &NogoodDescription,
    ) -> impl RetentionChecker<Predicate> + 'static {
        NogoodChecker {
            nogood: description.nogood.clone(),
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

    fn create_inference_checker(
        description: &NogoodDescription,
    ) -> impl InferenceChecker<Predicate> + 'static {
        UnitNogoodRule::create_inference_checker(description)
    }

    fn create_retention_checker(
        description: &NogoodDescription,
    ) -> impl RetentionChecker<Predicate> + 'static {
        ExtendedNogoodChecker {
            nogood: description.nogood.clone(),
        }
    }
}

impl PropagationMode {
    /// Register the rule with which the nogood propagator propagates `nogood` in this mode.
    ///
    /// Returns the [`InferenceCode`] of the nogood and the identifier of its retention checker,
    /// which the propagator removes when it deletes the nogood.
    pub(crate) fn register_nogood(
        self,
        state: &mut State,
        constraint_tag: ConstraintTag,
        nogood: &[Predicate],
        propagator: PropagatorId,
    ) -> (InferenceCode, Option<RetentionCheckerId>) {
        let description = NogoodDescription {
            nogood: nogood.into(),
        };

        match self {
            PropagationMode::UnitPropagation => state.register_removable_rule::<UnitNogoodRule>(
                constraint_tag,
                &description,
                propagator,
            ),
            PropagationMode::ExtendedNogoodPropagation => state
                .register_removable_rule::<ExtendedNogoodRule>(
                    constraint_tag,
                    &description,
                    propagator,
                ),
        }
    }
}
