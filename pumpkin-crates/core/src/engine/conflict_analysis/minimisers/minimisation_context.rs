use crate::containers::HashMap;
use crate::engine::Assignments;
use crate::engine::SolverStatistics;
use crate::engine::conflict_analysis::ConflictAnalysisContext;
#[cfg(doc)]
use crate::engine::propagation::ReadDomains;
use crate::engine::propagation::contexts::HasAssignments;
use crate::predicates::Predicate;
use crate::proof::InferenceCode;
use crate::proof::ProofLog;
use crate::state::CurrentNogood;
use crate::state::State;

/// The context used for during nogood minimisation.
///
/// Can be used to get the reason for a [`Predicate`] using
/// [`MinimisationContext::get_propagation_reason`], and information about integer variables and
/// [`Predicate`]s (see [`ReadDomains`]).
pub(crate) struct MinimisationContext<'a> {
    pub(crate) state: &'a mut State,

    pub(crate) proof_log: &'a mut ProofLog,
    pub(crate) unit_nogood_inference_codes: &'a HashMap<Predicate, InferenceCode>,

    pub(crate) counters: &'a mut SolverStatistics,
}
impl<'a> MinimisationContext<'a> {
    /// Compute the reason for `predicate` being true. The reason will be stored in
    /// `reason_buffer`.
    ///
    /// If `predicate` is not true, or it is a decision, then this function will panic.
    pub(crate) fn get_propagation_reason(
        &mut self,
        reason_buffer: &mut (impl Extend<Predicate> + AsRef<[Predicate]>),
        input_predicate: Predicate,
        current_nogood: CurrentNogood<'_>,
    ) {
        ConflictAnalysisContext::get_propagation_reason(
            input_predicate,
            current_nogood,
            self.proof_log,
            self.unit_nogood_inference_codes,
            reason_buffer,
            self.state,
        );
    }
}

impl<'a> HasAssignments for MinimisationContext<'a> {
    fn assignments(&self) -> &Assignments {
        &self.state.assignments
    }
}
