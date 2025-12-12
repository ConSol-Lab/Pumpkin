use std::fmt::Debug;

use super::minimisers::SemanticMinimiser;
use crate::Random;
use crate::basic_types::StoredConflictInfo;
use crate::branching::Brancher;
use crate::containers::HashMap;
use crate::engine::ConstraintSatisfactionSolver;
use crate::engine::EmptyDomainConflict;
use crate::engine::RestartStrategy;
use crate::engine::State;
use crate::engine::constraint_satisfaction_solver::CSPSolverState;
use crate::engine::predicates::predicate::Predicate;
use crate::engine::predicates::predicate::PredicateType;
use crate::engine::propagation::CurrentNogood;
use crate::engine::propagation::ExplanationContext;
use crate::engine::solver_statistics::SolverStatistics;
use crate::predicate;
use crate::predicates::PropositionalConjunction;
use crate::proof::InferenceCode;
use crate::proof::ProofLog;
use crate::proof::RootExplanationContext;
use crate::proof::explain_root_assignment;
use crate::propagators::nogoods::NogoodPropagator;
use crate::pumpkin_assert_eq_simple;
use crate::pumpkin_assert_simple;

/// Used during conflict analysis to provide the necessary information.
///
/// All fields are made public for the time being for simplicity. In the future that may change.
pub(crate) struct ConflictAnalysisContext<'a> {
    pub(crate) solver_state: &'a mut CSPSolverState,
    pub(crate) brancher: &'a mut dyn Brancher,
    pub(crate) semantic_minimiser: &'a mut SemanticMinimiser,

    pub(crate) counters: &'a mut SolverStatistics,

    pub(crate) proof_log: &'a mut ProofLog,
    pub(crate) should_minimise: bool,

    pub(crate) unit_nogood_inference_codes: &'a mut HashMap<Predicate, InferenceCode>,

    pub(crate) rng: &'a mut dyn Random,
    pub(crate) restart_strategy: &'a mut RestartStrategy,
    pub(crate) state: &'a mut State,
}

impl Debug for ConflictAnalysisContext<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct(std::any::type_name::<Self>()).finish()
    }
}

impl ConflictAnalysisContext<'_> {
    /// Returns the last decision which was made by the solver.
    pub(crate) fn find_last_decision(&mut self) -> Option<Predicate> {
        self.state.assignments.find_last_decision()
    }

    /// Posts the predicate with reason an empty reason.
    pub(crate) fn enqueue_propagated_predicate(&mut self, predicate: Predicate) {
        let update_occurred = self
            .state
            .post(predicate)
            .expect("Expected enqueued predicate to not lead to conflict directly");

        pumpkin_assert_simple!(
            update_occurred,
            "The propagated predicate should not already be true."
        );
    }

    /// Backtracks the solver to the provided backtrack level.
    pub(crate) fn backtrack(&mut self, backtrack_level: usize) {
        ConstraintSatisfactionSolver::backtrack(
            self.state,
            backtrack_level,
            self.brancher,
            self.rng,
        )
    }

    /// Returns a nogood which led to the conflict, excluding predicates from the root decision
    /// level.
    pub(crate) fn get_conflict_nogood(&mut self) -> Vec<Predicate> {
        let conflict_nogood = match self.solver_state.get_conflict_info() {
            StoredConflictInfo::Propagator(conflict) => {
                let _ = self.proof_log.log_inference(
                    &self.state.inference_codes,
                    conflict.inference_code,
                    conflict.conjunction.iter().copied(),
                    None,
                    self.state.variable_names(),
                );

                conflict.conjunction
            }
            StoredConflictInfo::EmptyDomain(conflict) => self.compute_conflict_nogood(conflict),
            StoredConflictInfo::RootLevelConflict(_) => {
                unreachable!("Should never attempt to learn a nogood from a root level conflict")
            }
            StoredConflictInfo::InconsistentAssumptions(predicate) => {
                vec![predicate, !predicate].into()
            }
        };

        for &predicate in conflict_nogood.iter() {
            let predicate_dl = self
                .state
                .get_checkpoint_for_predicate(predicate)
                .expect("all predicates in the conflict nogood should be assigned to true");

            if predicate_dl == 0 {
                explain_root_assignment(
                    &mut RootExplanationContext {
                        proof_log: self.proof_log,
                        unit_nogood_inference_codes: self.unit_nogood_inference_codes,
                        state: self.state,
                    },
                    predicate,
                );
            }
        }

        conflict_nogood
            .into_iter()
            .filter(|&p| self.state.get_checkpoint_for_predicate(p).unwrap() > 0)
            .collect()
    }

    /// Compute the reason for `predicate` being true. The reason will be stored in
    /// `reason_buffer`.
    ///
    /// If `predicate` is not true, or it is a decision, then this function will panic.
    pub(crate) fn get_propagation_reason(
        predicate: Predicate,
        current_nogood: CurrentNogood<'_>,
        proof_log: &mut ProofLog,
        unit_nogood_inference_codes: &HashMap<Predicate, InferenceCode>,
        reason_buffer: &mut (impl Extend<Predicate> + AsRef<[Predicate]>),
        state: &mut State,
    ) {
        let trail_index = state.get_propagation_reason(predicate, reason_buffer, current_nogood);

        if let Some(trail_index) = trail_index {
            let trail_entry = state.assignments.get_trail_entry(trail_index);
            let (reason_ref, inference_code) = trail_entry
                .reason
                .expect("Cannot be a null reason for propagation.");

            let propagator_id = state.reason_store.get_propagator(reason_ref);

            if state
                .propagators
                .as_propagator_handle::<NogoodPropagator>(propagator_id)
                .is_some()
                && reason_buffer.as_ref().is_empty()
            {
                // This means that a unit nogood was propagated, we indicate that this nogood step
                // was used
                //
                // It could be that the predicate is implied by another unit nogood

                let inference_code = unit_nogood_inference_codes
                    .get(&predicate)
                    .or_else(|| {
                        // It could be the case that we attempt to get the reason for the predicate
                        // [x >= v] but that the corresponding unit nogood idea is the one for the
                        // predicate [x == v]
                        let domain_id = predicate.get_domain();
                        let right_hand_side = predicate.get_right_hand_side();

                        unit_nogood_inference_codes.get(&predicate!(domain_id == right_hand_side))
                    })
                    .expect("Expected to be able to retrieve step id for unit nogood");

                let _ = proof_log.log_inference(
                    &state.inference_codes,
                    *inference_code,
                    [],
                    Some(predicate),
                    state.variable_names(),
                );
            } else {
                // Otherwise we log the inference which was used to derive the nogood
                let _ = proof_log.log_inference(
                    &state.inference_codes,
                    inference_code,
                    reason_buffer.as_ref().iter().copied(),
                    Some(predicate),
                    state.variable_names(),
                );
            }
        }
    }

    fn compute_conflict_nogood(
        &mut self,
        conflict: EmptyDomainConflict,
    ) -> PropositionalConjunction {
        let conflict_domain = conflict.domain();

        // Look up the reason for the bound that changed.
        // The reason for changing the bound cannot be a decision, so we can safely unwrap.
        let mut empty_domain_reason: Vec<Predicate> = vec![];
        let _ = self.state.reason_store.get_or_compute(
            conflict.trigger_reason,
            ExplanationContext::without_working_nogood(
                &self.state.assignments,
                self.state.assignments.num_trail_entries(), // Note that we do not do a
                // `-1` here; the `Assignments` automatically undoes the last trail entry when an
                // empty domain is created meaning that the `-1` has already been applied.
                &mut self.state.notification_engine,
            ),
            &mut self.state.propagators,
            &mut empty_domain_reason,
        );

        // We also need to log this last propagation to the proof log as an inference.
        let _ = self.proof_log.log_inference(
            &self.state.inference_codes,
            conflict.trigger_inference_code,
            empty_domain_reason.iter().copied(),
            Some(conflict.trigger_predicate),
            self.state.variable_names(),
        );

        let old_lower_bound = self.state.lower_bound(conflict_domain);
        let old_upper_bound = self.state.upper_bound(conflict_domain);

        match conflict.trigger_predicate.get_predicate_type() {
            PredicateType::LowerBound => {
                // The last trail entry was a lower-bound propagation meaning that the empty domain
                // was caused by the upper-bound
                //
                // We lift so that it is the most general upper-bound possible while still causing
                // the empty domain
                empty_domain_reason.push(predicate!(
                    conflict_domain <= conflict.trigger_predicate.get_right_hand_side() - 1
                ));
            }
            PredicateType::UpperBound => {
                // The last trail entry was an upper-bound propagation meaning that the empty domain
                // was caused by the lower-bound
                //
                // We lift so that it is the most general lower-bound possible while still causing
                // the empty domain
                empty_domain_reason.push(predicate!(
                    conflict_domain >= conflict.trigger_predicate.get_right_hand_side() + 1
                ));
            }
            PredicateType::NotEqual => {
                // The last trail entry was a not equals propagation meaning that the empty domain
                // was due to the domain being assigned to the removed value
                pumpkin_assert_eq_simple!(old_upper_bound, old_lower_bound);

                empty_domain_reason.push(predicate!(conflict_domain == old_lower_bound));
            }
            PredicateType::Equal => {
                // The last trail entry was an equality propagation; we split into three cases.
                if conflict.trigger_predicate.get_right_hand_side() < old_lower_bound {
                    // 1) The assigned value was lower than the lower-bound
                    //
                    // We lift so that it is the most general lower-bound possible while still
                    // causing the empty domain
                    empty_domain_reason.push(predicate!(
                        conflict_domain >= conflict.trigger_predicate.get_right_hand_side() + 1
                    ));
                } else if conflict.trigger_predicate.get_right_hand_side() > old_upper_bound {
                    // 2) The assigned value was larger than the upper-bound
                    //
                    // We lift so that it is the most general upper-bound possible while still
                    // causing the empty domain
                    empty_domain_reason.push(predicate!(
                        conflict_domain <= conflict.trigger_predicate.get_right_hand_side() - 1
                    ));
                } else {
                    // 3) The assigned value was equal to a hole in the domain
                    empty_domain_reason.push(predicate!(
                        conflict_domain != conflict.trigger_predicate.get_right_hand_side()
                    ))
                }
            }
        }

        empty_domain_reason.into()
    }
}
