#[cfg(feature = "inference-checkers")]
use crate::checkers::ConflictCheckerId;
use crate::checkers::ConflictCheckerStore;
#[cfg(feature = "check-retention")]
use crate::checkers::RetentionCheckerId;
#[cfg(feature = "check-retention")]
use crate::checkers::RetentionCheckerStore;
#[cfg(feature = "check-solutions")]
use crate::checkers::SolutionCheckerId;
#[cfg(feature = "check-solutions")]
use crate::checkers::SolutionCheckerStore;
use crate::proof::InferenceCode;

/// Owns the checkers of the rules registered in the solver: the conflict checkers, the retention
/// checkers and the solution checkers.
#[derive(Clone, Debug, Default)]
pub struct RuleCheckerStore {
    pub(crate) conflict_checkers: ConflictCheckerStore,
    #[cfg(feature = "check-retention")]
    pub(crate) retention_checkers: RetentionCheckerStore,
    #[cfg(feature = "check-solutions")]
    pub(crate) solution_checkers: SolutionCheckerStore,
}

impl RuleCheckerStore {
    /// Remove the checkers of a constraint that no longer exists.
    #[cfg(feature = "checkers")]
    pub(crate) fn remove(&mut self, rule_checkers: RemovableRuleCheckers) {
        #[cfg(feature = "inference-checkers")]
        if let Some(conflict_checker) = rule_checkers.conflict_checker {
            self.conflict_checkers
                .remove(rule_checkers.inference_code, conflict_checker);
        }

        #[cfg(feature = "check-retention")]
        if let Some(retention_checker) = rule_checkers.retention_checker {
            self.retention_checkers.remove(retention_checker);
        }

        #[cfg(feature = "check-solutions")]
        if let Some(solution_checker) = rule_checkers.solution_checker {
            self.solution_checkers.remove(solution_checker);
        }
    }
}

/// The checkers of a rule added for a constraint that can be removed, such as a learned nogood,
/// through which `RuleCheckerStore::remove` removes them.
///
/// The checkers exist only when their features are enabled.
#[derive(Clone, Copy, Debug)]
pub struct RemovableRuleCheckers {
    pub(crate) inference_code: InferenceCode,
    /// `None` when the rule is not checked; see `is_rule_checked`.
    #[cfg(feature = "inference-checkers")]
    pub(crate) conflict_checker: Option<ConflictCheckerId>,
    /// `None` when the rule is not checked; see `is_rule_checked`.
    #[cfg(feature = "check-retention")]
    pub(crate) retention_checker: Option<RetentionCheckerId>,
    /// `None` when the rule is not checked; see `is_rule_checked`.
    #[cfg(feature = "check-solutions")]
    pub(crate) solution_checker: Option<SolutionCheckerId>,
}

impl RemovableRuleCheckers {
    pub fn inference_code(&self) -> InferenceCode {
        self.inference_code
    }
}
