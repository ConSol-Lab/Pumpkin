use crate::containers::HashMap;
use crate::engine::Assignments;
use crate::engine::SolverStatistics;
use crate::engine::VariableNames;
use crate::engine::notifications::NotificationEngine;
use crate::engine::propagation::CurrentNogood;
use crate::engine::propagation::ExplanationContext;
use crate::engine::propagation::contexts::HasAssignments;
use crate::engine::propagation::store::PropagatorStore;
use crate::engine::reason::ReasonStore;
use crate::predicate;
use crate::predicates::Predicate;
use crate::predicates::PredicateType;
use crate::proof::InferenceCode;
use crate::proof::ProofLog;
use crate::pumpkin_assert_simple;

pub(crate) struct MinimisationContext<'a> {
    pub(crate) assignments: &'a Assignments,
    pub(crate) reason_store: &'a mut ReasonStore,
    pub(crate) notification_engine: &'a mut NotificationEngine,
    pub(crate) propagators: &'a mut PropagatorStore,

    pub(crate) proof_log: &'a mut ProofLog,
    pub(crate) unit_nogood_inference_codes: &'a HashMap<Predicate, InferenceCode>,
    pub(crate) variable_names: &'a VariableNames,

    pub(crate) counters: &'a mut SolverStatistics,
}

impl<'a> MinimisationContext<'a> {
    /// Compute the reason for `predicate` being true. The reason will be stored in
    /// `reason_buffer`.
    ///
    /// If `predicate` is not true, or it is a decision, then this function will panic.
    pub(crate) fn get_propagation_reason(
        &mut self,
        predicate: Predicate,
        reason_buffer: &mut (impl Extend<Predicate> + AsRef<[Predicate]>),
    ) {
        if self.assignments.is_initial_bound(predicate) {
            return;
        }

        let trail_position = self
            .assignments
            .get_trail_position(&predicate)
            .expect("The predicate must be true during conflict analysis.");

        let trail_entry = self.assignments.get_trail_entry(trail_position);

        // We distinguish between three cases:
        // 1) The predicate is explicitly present on the trail.
        if trail_entry.predicate == predicate {
            let (reason_ref, _) = trail_entry
                .reason
                .expect("Cannot be a null reason for propagation.");

            let explanation_context = ExplanationContext::new(
                self.assignments,
                CurrentNogood::empty(),
                trail_position,
                self.notification_engine,
            );

            let reason_exists = self.reason_store.get_or_compute(
                reason_ref,
                explanation_context,
                self.propagators,
                reason_buffer,
            );

            assert!(reason_exists, "reason reference should not be stale");
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
}

impl<'a> HasAssignments for MinimisationContext<'a> {
    fn assignments(&self) -> &Assignments {
        self.assignments
    }
}
