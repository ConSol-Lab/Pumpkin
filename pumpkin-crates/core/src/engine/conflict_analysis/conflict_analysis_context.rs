use std::fmt::Debug;

use super::minimisers::SemanticMinimiser;
use crate::Random;
use crate::basic_types::EmptyDomainConflict;
use crate::basic_types::StoredConflictInfo;
use crate::branching::Brancher;
use crate::containers::HashMap;
use crate::containers::StorageKey;
use crate::engine::Assignments;
use crate::engine::ConstraintSatisfactionSolver;
use crate::engine::PropagatorQueue;
use crate::engine::RestartStrategy;
use crate::engine::TrailedValues;
use crate::engine::VariableNames;
use crate::engine::constraint_satisfaction_solver::CSPSolverState;
use crate::engine::notifications::NotificationEngine;
use crate::engine::predicates::predicate::Predicate;
use crate::engine::predicates::predicate::PredicateType;
use crate::engine::propagation::CurrentNogood;
use crate::engine::propagation::ExplanationContext;
use crate::engine::propagation::store::PropagatorStore;
use crate::engine::reason::ReasonRef;
use crate::engine::reason::ReasonStore;
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
    pub(crate) assignments: &'a mut Assignments,
    pub(crate) solver_state: &'a mut CSPSolverState,
    pub(crate) reason_store: &'a mut ReasonStore,
    pub(crate) brancher: &'a mut dyn Brancher,
    pub(crate) propagators: &'a mut PropagatorStore,
    pub(crate) semantic_minimiser: &'a mut SemanticMinimiser,

    pub(crate) propagator_queue: &'a mut PropagatorQueue,

    pub(crate) notification_engine: &'a mut NotificationEngine,

    pub(crate) counters: &'a mut SolverStatistics,

    pub(crate) proof_log: &'a mut ProofLog,
    pub(crate) should_minimise: bool,

    pub(crate) unit_nogood_inference_codes: &'a mut HashMap<Predicate, InferenceCode>,
    pub(crate) trailed_values: &'a mut TrailedValues,
    pub(crate) variable_names: &'a VariableNames,

    pub(crate) rng: &'a mut dyn Random,
    pub(crate) restart_strategy: &'a mut RestartStrategy,
}

impl Debug for ConflictAnalysisContext<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct(std::any::type_name::<Self>()).finish()
    }
}

impl ConflictAnalysisContext<'_> {
    /// Returns the last decision which was made by the solver.
    pub(crate) fn find_last_decision(&mut self) -> Option<Predicate> {
        self.assignments.find_last_decision()
    }

    /// Posts the predicate with reason an empty reason.
    pub(crate) fn enqueue_propagated_predicate(&mut self, predicate: Predicate) {
        // This should only happen when we are not learning clauses. In that case, the proof log is
        // also nonsensical. So we can supply a garbage inference code.
        let garbage_inference_code = InferenceCode::create_from_index(0);

        let update_occurred = self
            .assignments
            .post_predicate(
                predicate,
                Some((ReasonRef(0), garbage_inference_code)),
                self.notification_engine,
            )
            .expect("Expected enqueued predicate to not lead to conflict directly");

        pumpkin_assert_simple!(
            update_occurred,
            "The propagated predicate should not already be true."
        );
    }

    /// Backtracks the solver to the provided backtrack level.
    pub(crate) fn backtrack(&mut self, backtrack_level: usize) {
        ConstraintSatisfactionSolver::backtrack(
            self.notification_engine,
            self.assignments,
            self.reason_store,
            self.propagator_queue,
            self.propagators,
            backtrack_level,
            self.brancher,
            self.trailed_values,
            self.rng,
        )
    }

    /// Returns a nogood which led to the conflict, excluding predicates from the root decision
    /// level.
    pub(crate) fn get_conflict_nogood(&mut self) -> Vec<Predicate> {
        let conflict_nogood = match self.solver_state.get_conflict_info() {
            StoredConflictInfo::Propagator(conflict) => {
                let _ = self.proof_log.log_inference(
                    conflict.inference_code,
                    conflict.conjunction.iter().copied(),
                    None,
                    self.variable_names,
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
                .assignments
                .get_decision_level_for_predicate(&predicate)
                .expect("all predicates in the conflict nogood should be assigned to true");

            if predicate_dl == 0 {
                explain_root_assignment(
                    &mut RootExplanationContext {
                        propagators: self.propagators,
                        proof_log: self.proof_log,
                        unit_nogood_inference_codes: self.unit_nogood_inference_codes,
                        assignments: self.assignments,
                        reason_store: self.reason_store,
                        notification_engine: self.notification_engine,
                        variable_names: self.variable_names,
                    },
                    predicate,
                );
            }
        }

        conflict_nogood
            .into_iter()
            .filter(|p| {
                self.assignments
                    .get_decision_level_for_predicate(p)
                    .unwrap()
                    > 0
            })
            .collect()
    }

    /// Compute the reason for `predicate` being true. The reason will be stored in
    /// `reason_buffer`.
    ///
    /// If `predicate` is not true, or it is a decision, then this function will panic.
    #[allow(
        clippy::too_many_arguments,
        reason = "borrow checker complains either here or elsewhere"
    )]
    pub(crate) fn get_propagation_reason(
        predicate: Predicate,
        assignments: &Assignments,
        current_nogood: CurrentNogood<'_>,
        reason_store: &mut ReasonStore,
        propagators: &mut PropagatorStore,
        proof_log: &mut ProofLog,
        unit_nogood_inference_codes: &HashMap<Predicate, InferenceCode>,
        reason_buffer: &mut (impl Extend<Predicate> + AsRef<[Predicate]>),
        notification_engine: &mut NotificationEngine,
        variable_names: &VariableNames,
    ) {
        // TODO: this function could be put into the reason store

        // Note that this function can only be called with propagations, and never decision
        // predicates. Furthermore only predicate from the current decision level will be
        // considered. This is due to how the 1uip conflict analysis works: it scans the
        // predicates in reverse order of assignment, and stops as soon as there is only one
        // predicate from the current decision level in the learned nogood.

        // This means that the procedure would never ask for the reason of the decision predicate
        // from the current decision level, because that would mean that all other predicates from
        // the current decision level have been removed from the nogood, and the decision
        // predicate is the only one left, but in that case, the 1uip would terminate since
        // there would be only one predicate from the current decision level. For this
        // reason, it is safe to assume that in the following, that any input predicate is
        // indeed a propagated predicate.
        if assignments.is_initial_bound(predicate) {
            return;
        }

        let trail_position = assignments
            .get_trail_position(&predicate)
            .expect("The predicate must be true during conflict analysis.");

        let trail_entry = assignments.get_trail_entry(trail_position);

        // We distinguish between three cases:
        // 1) The predicate is explicitly present on the trail.
        if trail_entry.predicate == predicate {
            let (reason_ref, inference_code) = trail_entry
                .reason
                .expect("Cannot be a null reason for propagation.");

            let propagator_id = reason_store.get_propagator(reason_ref);

            let explanation_context = ExplanationContext::new(
                assignments,
                current_nogood,
                trail_position,
                notification_engine,
            );

            let reason_exists = reason_store.get_or_compute(
                reason_ref,
                explanation_context,
                propagators,
                reason_buffer,
            );

            assert!(reason_exists, "reason reference should not be stale");

            if propagators
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

                let _ =
                    proof_log.log_inference(*inference_code, [], Some(predicate), variable_names);
            } else {
                // Otherwise we log the inference which was used to derive the nogood
                let _ = proof_log.log_inference(
                    inference_code,
                    reason_buffer.as_ref().iter().copied(),
                    Some(predicate),
                    variable_names,
                );
            }
        }
        // 2) The predicate is true due to a propagation, and not explicitly on the trail.
        // It is necessary to further analyse what was the reason for setting the predicate true.
        else {
            // The reason for propagation depends on:
            // 1) The predicate on the trail at the moment the input predicate became true, and
            // 2) The input predicate.
            match (
                trail_entry.predicate.get_predicate_type(),
                predicate.get_predicate_type(),
            ) {
                (PredicateType::LowerBound, PredicateType::LowerBound) => {
                    let trail_lower_bound = trail_entry.predicate.get_right_hand_side();
                    let domain_id = predicate.get_domain();
                    let input_lower_bound = predicate.get_right_hand_side();
                    // Both the input predicate and the trail predicate are lower bound
                    // literals. Two cases to consider:
                    // 1) The trail predicate has a greater right-hand side, meaning
                    //  the reason for the input predicate is true is because a stronger
                    //  right-hand side predicate was posted. We can reuse the same
                    //  reason as for the trail bound.
                    //  todo: could consider lifting here, since the trail bound
                    //  might be too strong.
                    if trail_lower_bound > input_lower_bound {
                        reason_buffer.extend(std::iter::once(trail_entry.predicate));
                    }
                    // Otherwise, the input bound is strictly greater than the trailed
                    // bound. This means the reason is due to holes in the domain.
                    else {
                        // Note that the bounds cannot be equal.
                        // If the bound were equal, the predicate would be explicitly on the
                        // trail, so we would have detected this case earlier.
                        pumpkin_assert_simple!(trail_lower_bound < input_lower_bound);

                        // The reason for the propagation of the input predicate [x >= a] is
                        // because [x >= a-1] & [x != a]. Conflict analysis will then
                        // recursively decompose these further.

                        // Note that we do not need to worry about decreasing the lower
                        // bounds so much so that it reaches its root lower bound, for which
                        // there is no reason since it is given as input to the problem.
                        // We cannot reach the original lower bound since in the 1uip, we
                        // only look for reasons for predicates from the current decision
                        // level, and we never look for reasons at the root level.

                        let one_less_bound_predicate =
                            predicate!(domain_id >= input_lower_bound - 1);

                        let not_equals_predicate = predicate!(domain_id != input_lower_bound - 1);
                        reason_buffer.extend(std::iter::once(one_less_bound_predicate));
                        reason_buffer.extend(std::iter::once(not_equals_predicate));
                    }
                }
                (PredicateType::LowerBound, PredicateType::NotEqual) => {
                    let trail_lower_bound = trail_entry.predicate.get_right_hand_side();
                    let not_equal_constant = predicate.get_right_hand_side();
                    // The trail entry is a lower bound literal,
                    // and the input predicate is a not equals.
                    // Only one case to consider:
                    // The trail lower bound is greater than the not_equals_constant,
                    // so it safe to take the reason from the trail.
                    // todo: lifting could be used here
                    pumpkin_assert_simple!(trail_lower_bound > not_equal_constant);
                    reason_buffer.extend(std::iter::once(trail_entry.predicate));
                }
                (PredicateType::LowerBound, PredicateType::Equal) => {
                    let domain_id = predicate.get_domain();
                    let equality_constant = predicate.get_right_hand_side();
                    // The input predicate is an equality predicate, and the trail predicate
                    // is a lower bound predicate. This means that the time of posting the
                    // trail predicate is when the input predicate became true.

                    // Note that the input equality constant does _not_ necessarily equal
                    // the trail lower bound. This would be the
                    // case when the the trail lower bound is lower than the input equality
                    // constant, but due to holes in the domain, the lower bound got raised
                    // to just the value of the equality constant.
                    // For example, {1, 2, 3, 10}, then posting [x >= 5] will raise the
                    // lower bound to x >= 10.

                    let predicate_lb = predicate!(domain_id >= equality_constant);
                    let predicate_ub = predicate!(domain_id <= equality_constant);
                    reason_buffer.extend(std::iter::once(predicate_lb));
                    reason_buffer.extend(std::iter::once(predicate_ub));
                }
                (PredicateType::UpperBound, PredicateType::UpperBound) => {
                    let trail_upper_bound = trail_entry.predicate.get_right_hand_side();
                    let domain_id = predicate.get_domain();
                    let input_upper_bound = predicate.get_right_hand_side();
                    // Both the input and trail predicates are upper bound predicates.
                    // There are two scenarios to consider:
                    // 1) The input upper bound is greater than the trail upper bound, meaning that
                    //    the reason for the input predicate is the propagation of a stronger upper
                    //    bound. We can safely use the reason for of the trail predicate as the
                    //    reason for the input predicate.
                    // todo: lifting could be applied here.
                    if trail_upper_bound < input_upper_bound {
                        reason_buffer.extend(std::iter::once(trail_entry.predicate));
                    } else {
                        // I think it cannot be that the bounds are equal, since otherwise we
                        // would have found the predicate explicitly on the trail.
                        pumpkin_assert_simple!(trail_upper_bound > input_upper_bound);

                        // The input upper bound is greater than the trail predicate, meaning
                        // that holes in the domain also played a rule in lowering the upper
                        // bound.

                        // The reason of the input predicate [x <= a] is computed recursively as
                        // the reason for [x <= a + 1] & [x != a + 1].

                        let new_ub_predicate = predicate!(domain_id <= input_upper_bound + 1);
                        let not_equal_predicate = predicate!(domain_id != input_upper_bound + 1);
                        reason_buffer.extend(std::iter::once(new_ub_predicate));
                        reason_buffer.extend(std::iter::once(not_equal_predicate));
                    }
                }
                (PredicateType::UpperBound, PredicateType::NotEqual) => {
                    let trail_upper_bound = trail_entry.predicate.get_right_hand_side();
                    let not_equal_constant = predicate.get_right_hand_side();
                    // The input predicate is a not equal predicate, and the trail predicate is
                    // an upper bound predicate. This is only possible when the upper bound was
                    // pushed below the not equals value. Otherwise the hole would have been
                    // explicitly placed on the trail and we would have found it earlier.
                    pumpkin_assert_simple!(not_equal_constant > trail_upper_bound);

                    // The bound was set past the not equals, so we can safely returns the trail
                    // reason. todo: can do lifting here.
                    reason_buffer.extend(std::iter::once(trail_entry.predicate));
                }
                (PredicateType::UpperBound, PredicateType::Equal) => {
                    let domain_id = predicate.get_domain();
                    let equality_constant = predicate.get_right_hand_side();
                    // The input predicate is an equality predicate, and the trail predicate
                    // is an upper bound predicate. This means that the time of posting the
                    // trail predicate is when the input predicate became true.

                    // Note that the input equality constant does _not_ necessarily equal
                    // the trail upper bound. This would be the
                    // case when the the trail upper bound is greater than the input equality
                    // constant, but due to holes in the domain, the upper bound got lowered
                    // to just the value of the equality constant.
                    // For example, x = {1, 2, 3, 8, 15}, setting [x <= 12] would lower the
                    // upper bound to x <= 8.

                    // Note that it could be that one of the two predicates are decision
                    // predicates, so we need to use the substitute functions.

                    let predicate_lb = predicate!(domain_id >= equality_constant);
                    let predicate_ub = predicate!(domain_id <= equality_constant);
                    reason_buffer.extend(std::iter::once(predicate_lb));
                    reason_buffer.extend(std::iter::once(predicate_ub));
                }
                (PredicateType::NotEqual, PredicateType::LowerBound) => {
                    let not_equal_constant = trail_entry.predicate.get_right_hand_side();
                    let domain_id = predicate.get_domain();
                    let input_lower_bound = predicate.get_right_hand_side();
                    // The trail predicate is not equals, but the input predicate is a lower
                    // bound predicate. This means that creating the hole in the domain resulted
                    // in raising the lower bound.

                    // I think this holds. The not_equals_constant cannot be greater, since that
                    // would not impact the lower bound. It can also not be the same, since
                    // creating a hole cannot result in the lower bound being raised to the
                    // hole, there must be some other reason for that to happen, which we would
                    // find earlier.
                    pumpkin_assert_simple!(input_lower_bound > not_equal_constant);

                    // The reason for the input predicate [x >= a] is computed recursively as
                    // the reason for [x >= a - 1] & [x != a - 1].
                    let new_lb_predicate = predicate!(domain_id >= input_lower_bound - 1);
                    let new_not_equals_predicate = predicate!(domain_id != input_lower_bound - 1);

                    reason_buffer.extend(std::iter::once(new_lb_predicate));
                    reason_buffer.extend(std::iter::once(new_not_equals_predicate));
                }
                (PredicateType::NotEqual, PredicateType::UpperBound) => {
                    let not_equal_constant = trail_entry.predicate.get_right_hand_side();
                    let domain_id = predicate.get_domain();
                    let input_upper_bound = predicate.get_right_hand_side();
                    // The trail predicate is not equals, but the input predicate is an upper
                    // bound predicate. This means that creating the hole in the domain resulted
                    // in lower the upper bound.

                    // I think this holds. The not_equals_constant cannot be smaller, since that
                    // would not impact the upper bound. It can also not be the same, since
                    // creating a hole cannot result in the upper bound being lower to the
                    // hole, there must be some other reason for that to happen, which we would
                    // find earlier.
                    pumpkin_assert_simple!(input_upper_bound < not_equal_constant);

                    // The reason for the input predicate [x <= a] is computed recursively as
                    // the reason for [x <= a + 1] & [x != a + 1].
                    let new_ub_predicate = predicate!(domain_id <= input_upper_bound + 1);
                    let new_not_equals_predicate = predicate!(domain_id != input_upper_bound + 1);

                    reason_buffer.extend(std::iter::once(new_ub_predicate));
                    reason_buffer.extend(std::iter::once(new_not_equals_predicate));
                }
                (PredicateType::NotEqual, PredicateType::Equal) => {
                    let domain_id = predicate.get_domain();
                    let equality_constant = predicate.get_right_hand_side();
                    // The trail predicate is not equals, but the input predicate is
                    // equals. The only time this could is when the not equals forces the
                    // lower/upper bounds to meet. So we simply look for the reasons for those
                    // bounds recursively.

                    // Note that it could be that one of the two predicates are decision
                    // predicates, so we need to use the substitute functions.

                    let predicate_lb = predicate!(domain_id >= equality_constant);
                    let predicate_ub = predicate!(domain_id <= equality_constant);

                    reason_buffer.extend(std::iter::once(predicate_lb));
                    reason_buffer.extend(std::iter::once(predicate_ub));
                }
                (
                    PredicateType::Equal,
                    PredicateType::LowerBound | PredicateType::UpperBound | PredicateType::NotEqual,
                ) => {
                    // The trail predicate is equality, but the input predicate is either a
                    // lower-bound, upper-bound, or not equals.
                    //
                    // TODO: could consider lifting here
                    reason_buffer.extend(std::iter::once(trail_entry.predicate))
                }
                _ => unreachable!(
                    "Unreachable combination of {} and {}",
                    trail_entry.predicate, predicate
                ),
            };
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
        let _ = self.reason_store.get_or_compute(
            conflict.trigger_reason,
            ExplanationContext::without_working_nogood(
                self.assignments,
                self.assignments.num_trail_entries() - 1,
                self.notification_engine,
            ),
            self.propagators,
            &mut empty_domain_reason,
        );

        // We also need to log this last propagation to the proof log as an inference.
        let _ = self.proof_log.log_inference(
            conflict.trigger_inference_code,
            empty_domain_reason.iter().copied(),
            Some(conflict.trigger_predicate),
            self.variable_names,
        );

        let old_lower_bound = self.assignments.get_lower_bound(conflict_domain);
        let old_upper_bound = self.assignments.get_upper_bound(conflict_domain);

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
