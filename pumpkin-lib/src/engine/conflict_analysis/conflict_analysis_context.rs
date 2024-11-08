use super::SemanticMinimiser;
use crate::basic_types::StoredConflictInfo;
use crate::branching::Brancher;
use crate::engine::constraint_satisfaction_solver::CSPSolverState;
use crate::engine::constraint_satisfaction_solver::Counters;
use crate::engine::predicates::predicate::Predicate;
use crate::engine::propagation::store::PropagatorStore;
use crate::engine::propagation::PropagationContext;
use crate::engine::reason::ReasonStore;
use crate::engine::Assignments;
use crate::engine::IntDomainEvent;
use crate::engine::PropagatorQueue;
use crate::engine::WatchListCP;
use crate::pumpkin_assert_simple;
use crate::variables::DomainId;

// Does not have debug because of the brancher does not support it. Could be thought through later.
#[allow(missing_debug_implementations)]
pub struct ConflictAnalysisNogoodContext<'a> {
    pub(crate) assignments: &'a mut Assignments,
    pub(crate) solver_state: &'a mut CSPSolverState,
    pub(crate) reason_store: &'a mut ReasonStore,
    pub(crate) counters: &'a mut Counters,
    pub(crate) brancher: &'a mut dyn Brancher,
    pub(crate) propagators: &'a mut PropagatorStore,
    pub(crate) semantic_minimiser: &'a mut SemanticMinimiser,

    pub(crate) last_notified_cp_trail_index: usize,
    pub(crate) watch_list_cp: &'a mut WatchListCP,
    pub(crate) propagator_queue: &'a mut PropagatorQueue,
    pub(crate) event_drain: &'a mut Vec<(IntDomainEvent, DomainId)>,

    pub(crate) backtrack_event_drain: &'a mut Vec<(IntDomainEvent, DomainId)>,
}

impl<'a> ConflictAnalysisNogoodContext<'a> {
    pub(crate) fn find_last_decision(&mut self) -> Option<Predicate> {
        self.assignments.decisions.last().copied()
    }

    pub(crate) fn enqueue_propagated_predicate(&mut self, predicate: Predicate) {
        self.assignments
            .post_predicate(predicate, None)
            .expect("Expected enqueued predicate to not lead to conflict directly")
    }

    pub(crate) fn backtrack(&mut self, backtrack_level: usize) {
        pumpkin_assert_simple!(backtrack_level < self.assignments.get_decision_level());

        self.brancher.on_backtrack();

        self.assignments
            .synchronise(
                backtrack_level,
                self.last_notified_cp_trail_index,
                self.watch_list_cp.is_watching_any_backtrack_events(),
            )
            .iter()
            .for_each(|(domain_id, previous_value)| {
                self.brancher
                    .on_unassign_integer(*domain_id, *previous_value)
            });
        self.last_notified_cp_trail_index = self.assignments.num_trail_entries();

        self.reason_store.synchronise(backtrack_level);
        self.propagator_queue.clear();
        // For now all propagators are called to synchronise, in the future this will be improved in
        // two ways:
        //      + allow incremental synchronisation
        //      + only call the subset of propagators that were notified since last backtrack
        for propagator in self.propagators.iter_propagators_mut() {
            let context = PropagationContext::new(self.assignments);
            propagator.synchronise(&context);
        }

        self.brancher.synchronise(self.assignments);
        let _ = self.process_backtrack_events();

        self.event_drain.clear();
    }

    fn process_backtrack_events(&mut self) -> bool {
        // If there are no variables being watched then there is no reason to perform these
        // operations
        if self.watch_list_cp.is_watching_any_backtrack_events() {
            self.backtrack_event_drain
                .extend(self.assignments.drain_backtrack_domain_events());

            if self.backtrack_event_drain.is_empty() {
                return false;
            }

            for (event, domain) in self.backtrack_event_drain.drain(..) {
                for propagator_var in self
                    .watch_list_cp
                    .get_backtrack_affected_propagators(event, domain)
                {
                    let propagator = &mut self.propagators[propagator_var.propagator];
                    let context = PropagationContext::new(self.assignments);

                    propagator.notify_backtrack(&context, propagator_var.variable, event.into())
                }
            }
        }
        true
    }

    pub(crate) fn get_conflict_nogood(&mut self) -> Vec<Predicate> {
        match self.solver_state.get_conflict_info() {
            StoredConflictInfo::Propagator {
                conflict_nogood,
                propagator_id: _,
            } => conflict_nogood
                .iter()
                .filter(|p| {
                    // filter out root predicates
                    self.assignments
                        .get_decision_level_for_predicate(p)
                        .is_some_and(|dl| dl > 0)
                })
                .copied()
                .collect(),
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

    pub(crate) fn get_propagation_reason_simple(
        predicate: Predicate,
        assignments: &Assignments,
        reason_store: &'a mut ReasonStore,
        propagators: &'a mut PropagatorStore,
    ) -> &'a [Predicate] {
        // probably this function should go into the reason store?

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

        // Helper function to extract the reason from the trail predicate.
        // We assume that the predicate is indeed propagated, and not a decision.
        // let mut extract_reason_from_trail =
        // |trail_entry: &ConstraintProgrammingTrailEntry| -> &PropositionalConjunction {
        // let propagation_context = PropagationContext::new(self.assignments);
        //
        // let reason_ref = trail_entry
        // .reason
        // .expect("Cannot be a null reason for propagation.");
        //
        // self.reason_store
        // .get_or_compute(reason_ref, &propagation_context)
        // .expect("reason reference should not be stale")
        // };

        let trail_position = assignments
            .get_trail_position(&predicate)
            .expect("The predicate must be true during conflict analysis.");

        let trail_entry = assignments.get_trail_entry(trail_position);

        // println!("input pred: {}", predicate);
        // println!("trail pred: {}", trail_entry.predicate);
        // println!(
        // "reason in loop: {:?}",
        // extract_reason_from_trail(&trail_entry)
        // );

        // We distinguish between three cases:
        // 1) The predicate is explicitly present on the trail.
        if trail_entry.predicate == predicate {
            // We can simply return the reason given on the trail.
            // extract_reason_from_trail(&trail_entry)
            let propagation_context = PropagationContext::new(assignments);

            let reason_ref = trail_entry
                .reason
                .expect("Cannot be a null reason for propagation.");

            reason_store
                .get_or_compute_new(reason_ref, &propagation_context, propagators)
                .expect("reason reference should not be stale")
        // The predicate is implicitly due as a result of a decision.
        }
        // 2) The predicate is true due to a propagation, and not explicitly on the trail.
        // It is necessary to further analyse what was the reason for setting the predicate true.
        else {
            // The reason for propagation depends on:
            // 1) The predicate on the trail at the moment the input predicate became true, and
            // 2) The input predicate.
            match trail_entry.predicate {
                Predicate::LowerBound {
                    domain_id: _,
                    lower_bound: trail_lower_bound,
                } => {
                    match predicate {
                        Predicate::LowerBound {
                            domain_id,
                            lower_bound: input_lower_bound,
                        } => {
                            // Both the input predicate and the trail predicate are lower bound
                            // literals. Two cases to consider:
                            // 1) The trail predicate has a greater right-hand side, meaning
                            //  the reason for the input predicate is true is because a stronger
                            //  right-hand side predicate was posted. We can reuse the same
                            //  reason as for the trail bound.
                            //  todo: could consider lifting here, since the trail bound
                            //  might be too strong.
                            if trail_lower_bound > input_lower_bound {
                                // We can simply return the reason given on the trail.
                                // extract_reason_from_trail(&trail_entry)
                                let propagation_context = PropagationContext::new(assignments);

                                if let Some(reason_ref) = trail_entry.reason {
                                    reason_store
                                        .get_or_compute_new(
                                            reason_ref,
                                            &propagation_context,
                                            propagators,
                                        )
                                        .expect("reason reference should not be stale")
                                }
                                // Otherwise the predicate is implicitly set due to a decision
                                else {
                                    reason_store.helper.clear();
                                    reason_store.helper.push(trail_entry.predicate);
                                    reason_store.helper.as_slice()
                                }
                            }
                            // Otherwise, the input bound is strictly greater than the trailed
                            // bound. This means the reason is due to holes in the domain.
                            else {
                                // Note that the bounds cannot be equal.
                                // If the bound were equal, the predicate would be explicitly on the
                                // trail, so we would have detected this case earlier.
                                // println!("{} {}", predicate, trail_entry.predicate);
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
                                reason_store.helper.clear();
                                reason_store.helper.push(one_less_bound_predicate);
                                reason_store.helper.push(not_equals_predicate);
                                reason_store.helper.as_slice()
                            }
                        }
                        Predicate::UpperBound {
                            domain_id: _,
                            upper_bound: _,
                        } => {
                            // The input predicate is an upper bound predicate, but the trail
                            // predicate is a lower bound predicate. This cannot be.
                            unreachable!()
                        }
                        Predicate::NotEqual {
                            domain_id: _,
                            not_equal_constant,
                        } => {
                            // The trail entry is a lower bound literal,
                            // and the input predicate is a not equals.
                            // Only one case to consider:
                            // The trail lower bound is greater than the not_equals_constant,
                            // so it safe to take the reason from the trail.
                            // todo: lifting could be used here
                            pumpkin_assert_simple!(trail_lower_bound > not_equal_constant);
                            // extract_reason_from_trail(&trail_entry)
                            let propagation_context = PropagationContext::new(assignments);

                            if let Some(reason_ref) = trail_entry.reason {
                                reason_store
                                    .get_or_compute_new(
                                        reason_ref,
                                        &propagation_context,
                                        propagators,
                                    )
                                    .expect("reason reference should not be stale")
                            }
                            // Otherwise the not equals predicate is due to a decision.
                            else {
                                reason_store.helper.clear();
                                reason_store.helper.push(trail_entry.predicate);
                                reason_store.helper.as_slice()
                            }
                        }
                        Predicate::Equal {
                            domain_id,
                            equality_constant,
                        } => {
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
                            reason_store.helper.clear();
                            reason_store.helper.push(predicate_lb);
                            reason_store.helper.push(predicate_ub);
                            reason_store.helper.as_slice()
                        }
                    }
                }
                Predicate::UpperBound {
                    domain_id: _,
                    upper_bound: trail_upper_bound,
                } => match predicate {
                    Predicate::LowerBound {
                        domain_id: _,
                        lower_bound: _,
                    } => {
                        // The input predicate is a lower bound predicate, and the trail predicate
                        // is an upper bound predicate. However this cannot be, since the lower
                        // bound cannot be implicitly because of an upper bound change.
                        // So we conclude this branch is unreachable.
                        unreachable!()
                    }
                    Predicate::UpperBound {
                        domain_id,
                        upper_bound: input_upper_bound,
                    } => {
                        // Both the input and trail predicates are upper bound predicates.
                        // There are two scenarios to consider:
                        // 1) The input upper bound is greater than the trail upper bound, meaning
                        //    that the reason for the input predicate is the propagation of a
                        //    stronger upper bound. We can safely use the reason for of the trail
                        //    predicate as the reason for the input predicate.
                        // todo: lifting could be applied here.
                        if trail_upper_bound < input_upper_bound {
                            // extract_reason_from_trail(&trail_entry)
                            let propagation_context = PropagationContext::new(assignments);

                            if let Some(reason_ref) = trail_entry.reason {
                                reason_store
                                    .get_or_compute_new(
                                        reason_ref,
                                        &propagation_context,
                                        propagators,
                                    )
                                    .expect("reason reference should not be stale")
                            } else {
                                reason_store.helper.clear();
                                reason_store.helper.push(trail_entry.predicate);
                                reason_store.helper.as_slice()
                            }
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
                            reason_store.helper.clear();
                            reason_store.helper.push(new_ub_predicate);
                            reason_store.helper.push(not_equal_predicate);
                            reason_store.helper.as_slice()
                        }
                    }
                    Predicate::NotEqual {
                        domain_id: _,
                        not_equal_constant,
                    } => {
                        // The input predicate is a not equal predicate, and the trail predicate is
                        // an upper bound predicate. This is only possible when the upper bound was
                        // pushed below the not equals value. Otherwise the hole would have been
                        // explicitly placed on the trail and we would have found it earlier.
                        pumpkin_assert_simple!(not_equal_constant > trail_upper_bound);

                        // The bound was set past the not equals, so we can safely returns the trail
                        // reason. todo: can do lifting here.
                        // extract_reason_from_trail(&trail_entry)
                        let propagation_context = PropagationContext::new(assignments);
                        if let Some(reason_ref) = trail_entry.reason {
                            reason_store
                                .get_or_compute_new(reason_ref, &propagation_context, propagators)
                                .expect("reason reference should not be stale")
                        } else {
                            reason_store.helper.clear();
                            reason_store.helper.push(trail_entry.predicate);
                            reason_store.helper.as_slice()
                        }
                    }
                    Predicate::Equal {
                        domain_id,
                        equality_constant,
                    } => {
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
                        reason_store.helper.clear();
                        reason_store.helper.push(predicate_lb);
                        reason_store.helper.push(predicate_ub);
                        reason_store.helper.as_slice()
                    }
                },
                Predicate::NotEqual {
                    domain_id: _,
                    not_equal_constant,
                } => match predicate {
                    Predicate::LowerBound {
                        domain_id,
                        lower_bound: input_lower_bound,
                    } => {
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

                        reason_store.helper.clear();
                        reason_store.helper.push(new_lb_predicate);
                        reason_store.helper.push(new_not_equals_predicate);
                        reason_store.helper.as_slice()
                    }
                    Predicate::UpperBound {
                        domain_id,
                        upper_bound: input_upper_bound,
                    } => {
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

                        reason_store.helper.clear();
                        reason_store.helper.push(new_ub_predicate);
                        reason_store.helper.push(new_not_equals_predicate);
                        reason_store.helper.as_slice()
                    }
                    Predicate::NotEqual {
                        domain_id: _,
                        not_equal_constant: _,
                    } => {
                        // Both the trail predicate and the input predicate are not equals.
                        // Note that if the two predicates were the same, then we would not get
                        // to this part of the code, since we would find the predicate explicitly on
                        // the trail. However the two predicates can be different, since not equals
                        // cannot imply another not equals. So we conclude this branch is
                        // unreachable.
                        unreachable!()
                    }
                    Predicate::Equal {
                        domain_id,
                        equality_constant,
                    } => {
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

                        reason_store.helper.clear();
                        reason_store.helper.push(predicate_lb);
                        reason_store.helper.push(predicate_ub);
                        reason_store.helper.as_slice()
                    }
                },
                Predicate::Equal {
                    domain_id: _,
                    equality_constant: _,
                } => {
                    // We do not post equality literals on the trail,
                    // and instead decomposed them into lower and upper bound predicates,
                    // so we can skip considering this case.
                    unreachable!()
                }
            }
        }
    }
}
