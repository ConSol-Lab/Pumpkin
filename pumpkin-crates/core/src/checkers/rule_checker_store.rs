#[cfg(feature = "check-propagations")]
use crate::checkers::ConflictCheckerId;
use crate::checkers::ConflictCheckerStore;
#[cfg(feature = "check-consistency")]
use crate::checkers::RetentionCheckerId;
#[cfg(feature = "check-consistency")]
use crate::checkers::RetentionCheckerStore;
use crate::proof::InferenceCode;

/// Owns the checkers of the rules registered in the solver: the conflict checkers and the
/// retention checkers.
#[derive(Clone, Debug, Default)]
pub struct RuleCheckerStore {
    pub(crate) conflict_checkers: ConflictCheckerStore,
    #[cfg(feature = "check-consistency")]
    pub(crate) retention_checkers: RetentionCheckerStore,
}

impl RuleCheckerStore {
    /// Remove both checkers of a constraint that no longer exists.
    #[cfg(any(feature = "check-propagations", feature = "check-consistency"))]
    pub(crate) fn remove(&mut self, rule_checkers: RemovableRuleCheckers) {
        #[cfg(feature = "check-propagations")]
        if let Some(conflict_checker) = rule_checkers.conflict_checker {
            self.conflict_checkers
                .remove(rule_checkers.inference_code, conflict_checker);
        }

        #[cfg(feature = "check-consistency")]
        if let Some(retention_checker) = rule_checkers.retention_checker {
            self.retention_checkers.remove(retention_checker);
        }
    }
}

/// The checkers of a rule added for a constraint that can be removed, such as a learned nogood,
/// through which `RuleCheckerStore::remove` removes them.
///
/// The checkers exist only when their features are enabled.
#[derive(Clone, Copy, Debug)]
pub struct RemovableRuleCheckers {
    /// The inference code of the constraint.
    pub(crate) inference_code: InferenceCode,
    /// `None` when the rule is not checked; see `is_rule_checked`.
    #[cfg(feature = "check-propagations")]
    pub(crate) conflict_checker: Option<ConflictCheckerId>,
    /// `None` when the rule is not checked; see `is_rule_checked`.
    #[cfg(feature = "check-consistency")]
    pub(crate) retention_checker: Option<RetentionCheckerId>,
}

impl RemovableRuleCheckers {
    /// The inference code of the constraint.
    pub fn inference_code(&self) -> InferenceCode {
        self.inference_code
    }
}
