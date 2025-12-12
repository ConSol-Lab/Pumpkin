use crate::containers::HashMap;
use crate::engine::Assignments;
use crate::engine::SolverStatistics;
use crate::engine::propagation::contexts::HasAssignments;
use crate::predicates::Predicate;
use crate::proof::InferenceCode;
use crate::proof::ProofLog;
use crate::state::State;

pub(crate) struct MinimisationContext<'a> {
    pub(crate) state: &'a mut State,

    pub(crate) proof_log: &'a mut ProofLog,
    pub(crate) unit_nogood_inference_codes: &'a HashMap<Predicate, InferenceCode>,

    pub(crate) counters: &'a mut SolverStatistics,
}

impl<'a> HasAssignments for MinimisationContext<'a> {
    fn assignments(&self) -> &Assignments {
        &self.state.assignments
    }
}
