//! The runtime checks of [`State`]. They are a debugging tool:
//! - the inference checks under `check-inferences` or `check-inferences-proof`, which run when a
//!   propagation is posted, when a propagator reports a conflict, or when conflict analysis
//!   computes a reason.

#[cfg(feature = "inference-checkers")]
use crate::checkers::Inference;
#[cfg(feature = "inference-checkers")]
use crate::checkers::check_inference;
#[cfg(feature = "inference-checkers")]
use crate::engine::reason::ReasonRef;
#[cfg(feature = "inference-checkers")]
use crate::state::PropagatorConflict;
use crate::state::State;

#[cfg(feature = "inference-checkers")]
impl State {
    /// Check an inference whose reason conflict analysis computed: every such inference under
    /// `check-inferences-proof`, and only those with a lazy reason under `check-inferences`, since
    /// the others were checked when they were posted.
    pub(crate) fn check_explained_inference(
        &self,
        reason_reference: ReasonRef,
        inference: Inference<'_>,
    ) {
        let is_lazy = self.reason_store.get_lazy_code(reason_reference).is_some();

        if cfg!(feature = "check-inferences-proof") || is_lazy {
            check_inference(
                &self.assignments,
                &self.rule_checkers.conflict_checkers,
                inference,
            );
        }
    }

    /// Check a conflict that a propagator reported: under `check-inferences` when the propagator
    /// returns it, under `check-inferences-proof` when conflict analysis starts from it.
    pub(crate) fn check_reported_conflict(&self, conflict: &PropagatorConflict) {
        check_inference(
            &self.assignments,
            &self.rule_checkers.conflict_checkers,
            Inference {
                premises: conflict.conjunction.as_slice(),
                consequent: None,
                consequent_position: None,
                inference_code: conflict.inference_code,
            },
        );
    }
}
