//! Responsible for finalizing the proof in the solver.
//!
//! The other resolvers are not fit for this job.

use drcp_format::steps::StepId;

use super::ProofLog;
use crate::basic_types::HashMap;
use crate::engine::conflict_analysis::ConflictAnalysisContext;
use crate::engine::propagation::store::PropagatorStore;
use crate::engine::propagation::CurrentNogood;
use crate::engine::reason::ReasonStore;
use crate::engine::Assignments;
use crate::predicates::Predicate;
use crate::predicates::PropositionalConjunction;

pub(crate) struct FinalizingContext<'a> {
    pub(crate) conflict: PropositionalConjunction,
    pub(crate) propagators: &'a mut PropagatorStore,
    pub(crate) proof_log: &'a mut ProofLog,
    pub(crate) unit_nogood_step_ids: &'a HashMap<Predicate, StepId>,
    pub(crate) assignments: &'a Assignments,
    pub(crate) reason_store: &'a mut ReasonStore,
}

/// Finalizes the proof by introducing inferences used to derive root-level unsatisfiability. This
/// happens by recursively going through the implication graph to explain any predicate that does
/// not have a nogood step id yet.
///
/// This should only include implicit propagations done through the [`Assignments`] struct. If a
/// predicate is propagated by a propagator, it would have been logged as a root-level propagation
/// by the solver prior to reaching this function.
pub(crate) fn finalize_proof(context: FinalizingContext<'_>) {
    if !context.proof_log.is_logging_inferences() {
        return;
    }

    for predicate in context.conflict {
        explain_root_assignment(
            &mut RootExplanationContext {
                propagators: context.propagators,
                proof_log: context.proof_log,
                unit_nogood_step_ids: context.unit_nogood_step_ids,
                assignments: context.assignments,
                reason_store: context.reason_store,
            },
            predicate,
        );
    }
}

pub(crate) struct RootExplanationContext<'a> {
    pub(crate) propagators: &'a mut PropagatorStore,
    pub(crate) proof_log: &'a mut ProofLog,
    pub(crate) unit_nogood_step_ids: &'a HashMap<Predicate, StepId>,
    pub(crate) assignments: &'a Assignments,
    pub(crate) reason_store: &'a mut ReasonStore,
}

/// Explain why a given predicate is true. We assume that `predicate` is true at the root.
pub(crate) fn explain_root_assignment(
    context: &mut RootExplanationContext<'_>,
    predicate: Predicate,
) {
    assert_eq!(
        context
            .assignments
            .get_decision_level_for_predicate(&predicate),
        Some(0)
    );

    // If the predicate is a root-level assignment, there is nothing to explain.
    if context.assignments.is_initial_bound(predicate) {
        return;
    }

    // If the predicate is a unit-nogood, we simply add that nogood step as a propagation.
    if let Some(id) = context.unit_nogood_step_ids.get(&predicate) {
        context.proof_log.add_propagation(*id);
        return;
    }

    // There must be some combination of other factors.
    let mut reason = vec![];
    ConflictAnalysisContext::get_propagation_reason(
        predicate,
        context.assignments,
        CurrentNogood::empty(),
        context.reason_store,
        context.propagators,
        context.proof_log,
        context.unit_nogood_step_ids,
        &mut reason,
    );

    assert!(!reason.is_empty());

    for p in reason {
        explain_root_assignment(context, p);
    }
}
