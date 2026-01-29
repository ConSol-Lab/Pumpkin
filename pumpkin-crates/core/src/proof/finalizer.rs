//! Responsible for finalizing the proof in the solver.
//!
//! The other resolvers are not fit for this job.

use std::collections::BTreeMap;

use super::InferenceCode;
use super::ProofLog;
use crate::conflict_resolving::ConflictAnalysisContext;
use crate::containers::HashMap;
use crate::containers::HashSet;
use crate::engine::State;
use crate::predicates::Predicate;
use crate::predicates::PropositionalConjunction;
use crate::propagation::CurrentNogood;

pub(crate) struct FinalizingContext<'a> {
    pub(crate) conflict: PropositionalConjunction,
    pub(crate) proof_log: &'a mut ProofLog,
    pub(crate) unit_nogood_inference_codes: &'a HashMap<Predicate, InferenceCode>,
    pub(crate) state: &'a mut State,
}

/// Finalizes the proof by introducing inferences used to derive root-level unsatisfiability. This
/// happens by recursively going through the implication graph to explain any predicate that does
/// not have a nogood step id yet.
///
/// This should only include implicit propagations done through the [`Assignments`] struct. If a
/// predicate is propagated by a propagator, it would have been logged as a root-level propagation
/// by the solver prior to reaching this function.
pub(crate) fn finalize_proof(context: FinalizingContext<'_>) {
    let mut to_explain = BTreeMap::new();
    to_explain.extend(context.conflict.iter().map(|&predicate| {
        (
            context
                .state
                .assignments
                .get_trail_position(&predicate)
                .expect("predicate is true"),
            [predicate].into_iter().collect(),
        )
    }));

    let final_nogood = finalize_proof_impl(
        &mut RootExplanationContext {
            proof_log: context.proof_log,
            unit_nogood_inference_codes: context.unit_nogood_inference_codes,
            state: context.state,
        },
        to_explain,
    );

    dbg!(&context.state.variable_names);

    let _ = context.proof_log.log_deduction(
        final_nogood,
        &context.state.variable_names,
        &mut context.state.constraint_tags,
        &context.state.assignments,
    );
}

fn finalize_proof_impl(
    context: &mut RootExplanationContext<'_>,
    mut to_explain: BTreeMap<usize, HashSet<Predicate>>,
) -> Vec<Predicate> {
    let mut required_assumptions = vec![];

    while let Some((current_trail_pos, predicates)) = to_explain.pop_last() {
        for predicate in predicates {
            dbg!(predicate);
            // If the predicate is a root-level assignment, add the appropriate inference to the
            // proof. No extra predicates need to be assumed.
            //
            // MUST be checked before `is_decision` as initial bounds are also marked as
            // decisions by the assignments.
            if context.state.assignments.is_initial_bound(predicate) {
                println!("  is initial bound");
                let _ = context.proof_log.log_domain_inference(
                    predicate,
                    &context.state.variable_names,
                    &mut context.state.constraint_tags,
                    &context.state.assignments,
                );
                continue;
            }

            // If the predicate is a decision, then it should be assumed true.
            if context.state.assignments.is_decision_predicate(&predicate) {
                println!("  is decision");
                required_assumptions.push(predicate);
                continue;
            }

            // If the predicate is a unit-nogood, we explain the root-level assignment.
            if let Some(inference_code) = context.unit_nogood_inference_codes.get(&predicate) {
                println!("  is unit nogood");
                let _ = context.proof_log.log_inference(
                    &mut context.state.constraint_tags,
                    inference_code.clone(),
                    [],
                    Some(predicate),
                    &context.state.variable_names,
                    &context.state.assignments,
                );
                continue;
            }

            // There must be some combination of other factors.
            let mut reason = vec![];
            ConflictAnalysisContext::get_propagation_reason_inner(
                predicate,
                CurrentNogood::empty(),
                context.proof_log,
                context.unit_nogood_inference_codes,
                &mut reason,
                context.state,
            );
            println!("  is propagation");
            dbg!(&reason);

            // Look for the reasons of the propagation premise.
            for predicate in reason {
                let trail_pos = context
                    .state
                    .trail_position(predicate)
                    .expect("predicate is true");

                assert!(trail_pos <= current_trail_pos);

                let predicates_at_position = to_explain.entry(trail_pos).or_default();
                let _ = predicates_at_position.insert(predicate);
            }
        }
    }

    required_assumptions
}

pub(crate) struct RootExplanationContext<'a> {
    pub(crate) proof_log: &'a mut ProofLog,
    pub(crate) unit_nogood_inference_codes: &'a HashMap<Predicate, InferenceCode>,
    pub(crate) state: &'a mut State,
}

/// Explain why a given predicate is true. We assume that `predicate` is true at the root.
pub(crate) fn explain_root_assignment(
    context: &mut RootExplanationContext<'_>,
    predicate: Predicate,
) {
    assert_eq!(
        context.state.get_checkpoint_for_predicate(predicate),
        Some(0)
    );

    if !context.proof_log.is_logging_inferences() {
        return;
    }

    let to_explain = BTreeMap::from([(
        context
            .state
            .trail_position(predicate)
            .expect("predicate is true"),
        [predicate].into_iter().collect(),
    )]);

    let _ = finalize_proof_impl(context, to_explain);
}
