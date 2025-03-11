use std::fmt::Debug;

use drcp_format::steps::StepId;

use super::minimisers::SemanticMinimiser;
use crate::basic_types::HashMap;
use crate::basic_types::StoredConflictInfo;
use crate::branching::Brancher;
use crate::engine::constraint_satisfaction_solver::CSPSolverState;
use crate::engine::predicates::predicate::Predicate;
use crate::engine::propagation::store::PropagatorStore;
use crate::engine::propagation::CurrentNogood;
use crate::engine::propagation::ExplanationContext;
use crate::engine::reason::ReasonRef;
use crate::engine::reason::ReasonStore;
use crate::engine::solver_statistics::SolverStatistics;
use crate::engine::Assignments;
use crate::engine::ConstraintSatisfactionSolver;
use crate::engine::IntDomainEvent;
use crate::engine::PropagatorQueue;
use crate::engine::TrailedAssignments;
use crate::engine::WatchListCP;
use crate::predicate;
use crate::proof::ProofLog;
use crate::pumpkin_assert_simple;
use crate::variables::DomainId;

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

    pub(crate) last_notified_cp_trail_index: &'a mut usize,
    pub(crate) watch_list_cp: &'a mut WatchListCP,
    pub(crate) propagator_queue: &'a mut PropagatorQueue,
    pub(crate) event_drain: &'a mut Vec<(IntDomainEvent, DomainId)>,

    pub(crate) backtrack_event_drain: &'a mut Vec<(IntDomainEvent, DomainId)>,
    pub(crate) counters: &'a mut SolverStatistics,

    pub(crate) proof_log: &'a mut ProofLog,
    pub(crate) should_minimise: bool,

    pub(crate) unit_nogood_step_ids: &'a HashMap<Predicate, StepId>,
    pub(crate) stateful_assignments: &'a mut TrailedAssignments,
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
        self.assignments
            .post_predicate(predicate, Some(ReasonRef(0)))
            .expect("Expected enqueued predicate to not lead to conflict directly")
    }

    /// Backtracks the solver to the provided backtrack level.
    pub(crate) fn backtrack(&mut self, backtrack_level: usize) {
        ConstraintSatisfactionSolver::backtrack(
            self.assignments,
            self.last_notified_cp_trail_index,
            self.reason_store,
            self.propagator_queue,
            self.watch_list_cp,
            self.propagators,
            self.event_drain,
            self.backtrack_event_drain,
            backtrack_level,
            self.brancher,
            self.stateful_assignments,
        )
    }

    /// Returns a nogood which led to the conflict, excluding predicates from the root decision
    /// level.
    pub(crate) fn get_conflict_nogood(&mut self) -> Vec<Predicate> {
        match self.solver_state.get_conflict_info() {
            StoredConflictInfo::Propagator {
                conflict_nogood,
                propagator_id,
            } => {
                let _ = self.proof_log.log_inference(
                    self.propagators.get_tag(propagator_id),
                    conflict_nogood.iter().copied(),
                    None,
                );
                conflict_nogood
                    .iter()
                    .filter(|p| {
                        // filter out root predicates
                        self.assignments
                            .get_decision_level_for_predicate(p)
                            .is_some_and(|dl| dl > 0)
                    })
                    .copied()
                    .collect()
            }
            StoredConflictInfo::EmptyDomain { conflict_nogood } => {
                conflict_nogood
                    .iter()
                    .filter(|p| {
                        // filter out root predicates
                        self.assignments
                            .get_decision_level_for_predicate(p)
                            .is_some_and(|dl| dl > 0)
                    })
                    .copied()
                    .collect()
            }
            StoredConflictInfo::RootLevelConflict(_) => {
                unreachable!("Should never attempt to learn a nogood from a root level conflict")
            }
        }
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
        unit_nogood_step_ids: &HashMap<Predicate, StepId>,
        reason_buffer: &mut (impl Extend<Predicate> + AsRef<[Predicate]>),
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
            let reason_ref = trail_entry
                .reason
                .expect("Cannot be a null reason for propagation.");

            let propagator_id = reason_store.get_propagator(reason_ref);
            let constraint_tag = propagators.get_tag(propagator_id);

            let explanation_context = ExplanationContext::new(assignments, current_nogood);

            let reason_exists = reason_store.get_or_compute(
                reason_ref,
                explanation_context,
                propagators,
                reason_buffer,
            );

            assert!(reason_exists, "reason reference should not be stale");

            if propagator_id == ConstraintSatisfactionSolver::get_nogood_propagator_id()
                && reason_buffer.as_ref().is_empty()
            {
                // This means that a unit nogood was propagated, we indicate that this nogood step
                // was used
                //
                // It could be that the predicate is implied by another unit nogood

                let step_id = unit_nogood_step_ids
                    .get(&predicate)
                    .or_else(|| {
                        // It could be the case that we attempt to get the reason for the predicate
                        // [x >= v] but that the corresponding unit nogood idea is the one for the
                        // predicate [x == v]
                        let domain_id = predicate.get_domain();
                        let right_hand_side = predicate.get_right_hand_side();

                        unit_nogood_step_ids.get(&predicate!(domain_id == right_hand_side))
                    })
                    .expect("Expected to be able to retrieve step id for unit nogood");
                proof_log.add_propagation(*step_id);
            } else {
                // Otherwise we log the inference which was used to derive the nogood
                let _ = proof_log.log_inference(
                    constraint_tag,
                    reason_buffer.as_ref().iter().copied(),
                    Some(predicate),
                );
            }
        }
        // 2) The predicate is true due to a propagation, and not explicitly on the trail.
        // It is necessary to further analyse what was the reason for setting the predicate true.
        else {
            // The reason for propagation depends on:
            // 1) The predicate on the trail at the moment the input predicate became true, and
            // 2) The input predicate.
            match (trail_entry.predicate, predicate) {
                (
                    Predicate::LowerBound {
                        domain_id: _,
                        lower_bound: trail_lower_bound,
                    },
                    Predicate::LowerBound {
                        domain_id,
                        lower_bound: input_lower_bound,
                    },
                ) => {
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

                        let one_less_bound_predicate = Predicate::LowerBound {
                            domain_id,
                            lower_bound: input_lower_bound - 1,
                        };

                        let not_equals_predicate = Predicate::NotEqual {
                            domain_id,
                            not_equal_constant: input_lower_bound - 1,
                        };
                        reason_buffer.extend(std::iter::once(one_less_bound_predicate));
                        reason_buffer.extend(std::iter::once(not_equals_predicate));
                    }
                }
                (
                    Predicate::LowerBound {
                        domain_id: _,
                        lower_bound: trail_lower_bound,
                    },
                    Predicate::NotEqual {
                        domain_id: _,
                        not_equal_constant,
                    },
                ) => {
                    // The trail entry is a lower bound literal,
                    // and the input predicate is a not equals.
                    // Only one case to consider:
                    // The trail lower bound is greater than the not_equals_constant,
                    // so it safe to take the reason from the trail.
                    // todo: lifting could be used here
                    pumpkin_assert_simple!(trail_lower_bound > not_equal_constant);
                    reason_buffer.extend(std::iter::once(trail_entry.predicate));
                }
                (
                    Predicate::LowerBound {
                        domain_id: _,
                        lower_bound: _,
                    },
                    Predicate::Equal {
                        domain_id,
                        equality_constant,
                    },
                ) => {
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

                    let predicate_lb = Predicate::LowerBound {
                        domain_id,
                        lower_bound: equality_constant,
                    };
                    let predicate_ub = Predicate::UpperBound {
                        domain_id,
                        upper_bound: equality_constant,
                    };
                    reason_buffer.extend(std::iter::once(predicate_lb));
                    reason_buffer.extend(std::iter::once(predicate_ub));
                }
                (
                    Predicate::UpperBound {
                        domain_id: _,
                        upper_bound: trail_upper_bound,
                    },
                    Predicate::UpperBound {
                        domain_id,
                        upper_bound: input_upper_bound,
                    },
                ) => {
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

                        let new_ub_predicate = Predicate::UpperBound {
                            domain_id,
                            upper_bound: input_upper_bound + 1,
                        };
                        let not_equal_predicate = Predicate::NotEqual {
                            domain_id,
                            not_equal_constant: input_upper_bound + 1,
                        };
                        reason_buffer.extend(std::iter::once(new_ub_predicate));
                        reason_buffer.extend(std::iter::once(not_equal_predicate));
                    }
                }
                (
                    Predicate::UpperBound {
                        domain_id: _,
                        upper_bound: trail_upper_bound,
                    },
                    Predicate::NotEqual {
                        domain_id: _,
                        not_equal_constant,
                    },
                ) => {
                    // The input predicate is a not equal predicate, and the trail predicate is
                    // an upper bound predicate. This is only possible when the upper bound was
                    // pushed below the not equals value. Otherwise the hole would have been
                    // explicitly placed on the trail and we would have found it earlier.
                    pumpkin_assert_simple!(not_equal_constant > trail_upper_bound);

                    // The bound was set past the not equals, so we can safely returns the trail
                    // reason. todo: can do lifting here.
                    reason_buffer.extend(std::iter::once(trail_entry.predicate));
                }
                (
                    Predicate::UpperBound {
                        domain_id: _,
                        upper_bound: _,
                    },
                    Predicate::Equal {
                        domain_id,
                        equality_constant,
                    },
                ) => {
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

                    let predicate_lb = Predicate::LowerBound {
                        domain_id,
                        lower_bound: equality_constant,
                    };
                    let predicate_ub = Predicate::UpperBound {
                        domain_id,
                        upper_bound: equality_constant,
                    };
                    reason_buffer.extend(std::iter::once(predicate_lb));
                    reason_buffer.extend(std::iter::once(predicate_ub));
                }
                (
                    Predicate::NotEqual {
                        domain_id: _,
                        not_equal_constant,
                    },
                    Predicate::LowerBound {
                        domain_id,
                        lower_bound: input_lower_bound,
                    },
                ) => {
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
                    let new_lb_predicate = Predicate::LowerBound {
                        domain_id,
                        lower_bound: input_lower_bound - 1,
                    };
                    let new_not_equals_predicate = Predicate::NotEqual {
                        domain_id,
                        not_equal_constant: input_lower_bound - 1,
                    };

                    reason_buffer.extend(std::iter::once(new_lb_predicate));
                    reason_buffer.extend(std::iter::once(new_not_equals_predicate));
                }
                (
                    Predicate::NotEqual {
                        domain_id: _,
                        not_equal_constant,
                    },
                    Predicate::UpperBound {
                        domain_id,
                        upper_bound: input_upper_bound,
                    },
                ) => {
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
                    let new_ub_predicate = Predicate::UpperBound {
                        domain_id,
                        upper_bound: input_upper_bound + 1,
                    };
                    let new_not_equals_predicate = Predicate::NotEqual {
                        domain_id,
                        not_equal_constant: input_upper_bound + 1,
                    };

                    reason_buffer.extend(std::iter::once(new_ub_predicate));
                    reason_buffer.extend(std::iter::once(new_not_equals_predicate));
                }
                (
                    Predicate::NotEqual {
                        domain_id: _,
                        not_equal_constant: _,
                    },
                    Predicate::Equal {
                        domain_id,
                        equality_constant,
                    },
                ) => {
                    // The trail predicate is not equals, but the input predicate is
                    // equals. The only time this could is when the not equals forces the
                    // lower/upper bounds to meet. So we simply look for the reasons for those
                    // bounds recursively.

                    // Note that it could be that one of the two predicates are decision
                    // predicates, so we need to use the substitute functions.

                    let predicate_lb = Predicate::LowerBound {
                        domain_id,
                        lower_bound: equality_constant,
                    };
                    let predicate_ub = Predicate::UpperBound {
                        domain_id,
                        upper_bound: equality_constant,
                    };

                    reason_buffer.extend(std::iter::once(predicate_lb));
                    reason_buffer.extend(std::iter::once(predicate_ub));
                }
                _ => unreachable!(
                    "Unreachable combination of {} and {}",
                    trail_entry.predicate, predicate
                ),
            };
        }
    }
}
