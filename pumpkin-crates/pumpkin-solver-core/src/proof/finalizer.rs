//! Responsible for finalizing the proof in the solver.
//!
//! The other resolvers are not fit for this job.

use super::InferenceCode;
use super::ProofLog;
use crate::basic_types::HashMap;
use crate::engine::conflict_analysis::ConflictAnalysisContext;
use crate::engine::propagation::store::PropagatorStore;
use crate::engine::propagation::CurrentNogood;
use crate::engine::reason::ReasonStore;
use crate::engine::Assignments;
use crate::engine::VariableNames;
use crate::predicates::Predicate;
use crate::predicates::PropositionalConjunction;

pub(crate) struct FinalizingContext<'a> {
    pub(crate) conflict: PropositionalConjunction,
    pub(crate) propagators: &'a mut PropagatorStore,
    pub(crate) proof_log: &'a mut ProofLog,
    pub(crate) unit_nogood_inference_codes: &'a HashMap<Predicate, InferenceCode>,
    pub(crate) assignments: &'a Assignments,
    pub(crate) reason_store: &'a mut ReasonStore,
    pub(crate) variable_names: &'a VariableNames,
}

/// Finalizes the proof by introducing inferences used to derive root-level unsatisfiability. This
/// happens by recursively going through the implication graph to explain any predicate that does
/// not have a nogood step id yet.
///
/// This should only include implicit propagations done through the [`Assignments`] struct. If a
/// predicate is propagated by a propagator, it would have been logged as a root-level propagation
/// by the solver prior to reaching this function.
pub(crate) fn finalize_proof(context: FinalizingContext<'_>) {
    let final_nogood = context
        .conflict
        .into_iter()
        .flat_map(|predicate| {
            get_required_assumptions(
                &mut RootExplanationContext {
                    propagators: context.propagators,
                    proof_log: context.proof_log,
                    unit_nogood_inference_codes: context.unit_nogood_inference_codes,
                    assignments: context.assignments,
                    reason_store: context.reason_store,
                    variable_names: context.variable_names,
                },
                predicate,
            )
        })
        .collect::<Vec<_>>();

    let _ = context
        .proof_log
        .log_deduction(final_nogood, context.variable_names);
}

pub(crate) struct RootExplanationContext<'a> {
    pub(crate) propagators: &'a mut PropagatorStore,
    pub(crate) proof_log: &'a mut ProofLog,
    pub(crate) unit_nogood_inference_codes: &'a HashMap<Predicate, InferenceCode>,
    pub(crate) assignments: &'a Assignments,
    pub(crate) reason_store: &'a mut ReasonStore,
    pub(crate) variable_names: &'a VariableNames,
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

    if !context.proof_log.is_logging_inferences() {
        return;
    }

    let _ = get_required_assumptions(context, predicate);
}

/// Returns the predicates that should be assumed are true to make the given predicate true. It may
/// be that the given predicate is part of that list. Any propagations encountered along the way
/// are logged to the proof.
///
/// Note this function is private, as we do not expect it to be called outside this file. Returning
/// vectors will probably add allocations, but they will be small and this is not called in the
/// solver's hot loop so it should be fine.
fn get_required_assumptions(
    context: &mut RootExplanationContext<'_>,
    predicate: Predicate,
) -> Vec<Predicate> {
    if context.assignments.is_decision_predicate(&predicate) {
        return vec![predicate];
    }

    // If the predicate is a root-level assignment, add the appropriate inference to the proof.
    if context.assignments.is_initial_bound(predicate) {
        let _ = context
            .proof_log
            .log_domain_inference(predicate, context.variable_names);
        return vec![];
    }

    // If the predicate is a unit-nogood, we explain the root-level assignment.
    if let Some(inference_code) = context.unit_nogood_inference_codes.get(&predicate) {
        let _ = context.proof_log.log_inference(
            *inference_code,
            [],
            Some(predicate),
            context.variable_names,
        );
        return vec![];
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
        context.unit_nogood_inference_codes,
        &mut reason,
        context.variable_names,
    );

    // Here we combine all the required assumptions of recursive reasons.
    reason
        .into_iter()
        .flat_map(|predicate| get_required_assumptions(context, predicate))
        .collect()
}
