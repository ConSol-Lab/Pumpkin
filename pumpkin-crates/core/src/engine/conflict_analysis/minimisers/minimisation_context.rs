use crate::basic_types::moving_averages::MovingAverage;
use crate::containers::HashMap;
use crate::engine::Assignments;
use crate::engine::SolverStatistics;
use crate::engine::conflict_analysis::ConflictAnalysisContext;
use crate::predicates::Predicate;
use crate::proof::InferenceCode;
use crate::proof::ProofLog;
use crate::proof::RootExplanationContext;
use crate::proof::explain_root_assignment;
use crate::propagation::HasAssignments;
#[cfg(doc)]
use crate::propagation::ReadDomains;
use crate::state::CurrentNogood;
use crate::state::State;

/// The context used for during nogood minimisation.
///
/// Can be used to get the reason for a [`Predicate`] using
/// [`MinimisationContext::get_propagation_reason`], and information about integer variables and
/// [`Predicate`]s (see [`ReadDomains`]).
#[derive(Debug)]
pub struct MinimisationContext<'a> {
    pub(crate) state: &'a mut State,

    pub proof_log: &'a mut ProofLog,
    pub(crate) unit_nogood_inference_codes: &'a HashMap<Predicate, InferenceCode>,

    pub(crate) counters: &'a mut SolverStatistics,
}

impl<'a> MinimisationContext<'a> {
    pub fn new(
        state: &'a mut State,
        proof_log: &'a mut ProofLog,
        unit_nogood_inference_codes: &'a HashMap<Predicate, InferenceCode>,
        counters: &'a mut SolverStatistics,
    ) -> Self {
        Self {
            state,
            proof_log,
            unit_nogood_inference_codes,
            counters,
        }
    }
    /// Compute the reason for `predicate` being true. The reason will be stored in
    /// `reason_buffer`.
    ///
    /// If `predicate` is not true, or it is a decision, then this function will panic.
    pub fn get_propagation_reason(
        &mut self,
        reason_buffer: &mut (impl Extend<Predicate> + AsRef<[Predicate]>),
        input_predicate: Predicate,
        current_nogood: CurrentNogood<'_>,
    ) {
        ConflictAnalysisContext::get_propagation_reason_inner(
            input_predicate,
            current_nogood,
            self.proof_log,
            self.unit_nogood_inference_codes,
            reason_buffer,
            self.state,
        );
    }

    pub fn explain_root_assignment(&mut self, predicate: Predicate) {
        explain_root_assignment(
            &mut RootExplanationContext {
                proof_log: &mut self.proof_log,
                unit_nogood_inference_codes: &mut self.unit_nogood_inference_codes,
                state: &mut self.state,
            },
            predicate,
        );
    }

    pub fn removed_predicates_by_recursive(&mut self, num_predicates_removed: usize) {
        self.counters
            .learned_clause_statistics
            .average_number_of_removed_atomic_constraints_recursive
            .add_term(num_predicates_removed as u64);
    }
}

impl<'a> HasAssignments for MinimisationContext<'a> {
    fn assignments(&self) -> &Assignments {
        &self.state.assignments
    }

    fn trailed_values(&self) -> &crate::engine::TrailedValues {
        &self.state.trailed_values
    }

    fn trailed_values_mut(&mut self) -> &mut crate::engine::TrailedValues {
        &mut self.state.trailed_values
    }
}
