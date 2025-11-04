use std::num::NonZero;

use log::debug;
use log::trace;

use crate::basic_types::StoredConflictInfo;
use crate::basic_types::moving_averages::CumulativeMovingAverage;
use crate::basic_types::moving_averages::MovingAverage;
use crate::create_statistics_struct;
use crate::engine::Assignments;
use crate::engine::ConstraintProgrammingTrailEntry;
use crate::engine::conflict_analysis::ConflictAnalysisContext;
use crate::engine::conflict_analysis::ConflictResolver;
use crate::engine::propagation::constructor::PropagatorConstructor;
use crate::engine::propagation::constructor::PropagatorConstructorContext;
use crate::engine::propagation::store::PropagatorStore;
use crate::engine::reason::ReasonRef;
use crate::engine::reason::ReasonStore;
use crate::math::num_ext::NumExt;
use crate::predicate;
use crate::predicates::Predicate;
use crate::propagators::HypercubeLinear;
use crate::propagators::HypercubeLinearPropagator;
use crate::propagators::HypercubeLinearPropagatorArgs;
use crate::statistics::Statistic;
use crate::variables::DomainId;

create_statistics_struct!(HypercubeLinearResolutionStatistics {
    average_num_linear_terms_in_learned_hypercube_linear: CumulativeMovingAverage<usize>,
    num_learned_hypercube_linear: usize,
});

#[derive(Debug, Default)]
pub(crate) struct HypercubeLinearResolver {
    statistics: HypercubeLinearResolutionStatistics,
}

impl HypercubeLinearResolver {
    fn compute_conflicting_hypercube_linear(
        &self,
        context: &mut ConflictAnalysisContext<'_>,
    ) -> HypercubeLinear {
        let StoredConflictInfo::EmptyDomainTwo {
            conflict_trigger_reason,
            ..
        } = context.solver_state.get_conflict_info()
        else {
            panic!(
                "Cannot start hypercube analysis from {:?}",
                context.solver_state.get_conflict_info()
            );
        };

        let propagator_id = context.reason_store.get_propagator(conflict_trigger_reason);
        let handle = context
            .propagators
            .as_propagator_handle::<HypercubeLinearPropagator>(propagator_id)
            .expect("all propagations by hypercube propagator");

        context
            .propagators
            .get_propagator(handle)
            .expect("we already tested that it is an affine view")
            .hypercube_linear
            .clone()
    }

    fn restore_solver(
        &mut self,
        mut context: ConflictAnalysisContext<'_>,
        decision_level: usize,
        learned_constraint: HypercubeLinear,
    ) {
        debug!("Learned {learned_constraint}");
        debug!(
            "Jumping to decision level {decision_level} from decision level {}",
            context.assignments.get_decision_level()
        );

        self.statistics
            .average_num_linear_terms_in_learned_hypercube_linear
            .add_term(learned_constraint.iter_linear_terms().len());
        self.statistics.num_learned_hypercube_linear += 1;

        context.counters.engine_statistics.num_backjumps += 1;
        context
            .counters
            .learned_clause_statistics
            .average_backtrack_amount
            .add_term((context.assignments.get_decision_level() - decision_level) as u64);

        context.backtrack(decision_level);

        let constraint_tag = context.proof_log.new_constraint_tag();

        let propagator_slot = context.propagators.new_propagator();

        let constructor_context = PropagatorConstructorContext::new(
            context.notification_engine,
            context.trailed_values,
            context.proof_log,
            propagator_slot.key().propagator_id(),
            context.assignments,
        );

        let propagator = HypercubeLinearPropagatorArgs {
            hypercube_linear: learned_constraint,
            constraint_tag,
        }
        .create(constructor_context);

        let new_propagator_id = propagator_slot.populate(propagator);

        let new_propagator = &mut context.propagators[new_propagator_id.propagator_id()];

        context
            .propagator_queue
            .enqueue_propagator(new_propagator_id.propagator_id(), new_propagator.priority());

        context.solver_state.declare_solving();
    }
}

impl ConflictResolver for HypercubeLinearResolver {
    fn log_statistics(&self, statistic_logger: crate::statistics::StatisticLogger) {
        self.statistics.log(statistic_logger);
    }

    fn resolve_conflict(&mut self, mut context: ConflictAnalysisContext) -> bool {
        let mut conflicting_hypercube_linear =
            self.compute_conflicting_hypercube_linear(&mut context);

        debug!(
            "Resolving conflict (dl = {}) {conflicting_hypercube_linear}",
            context.assignments.get_decision_level()
        );

        let mut trail_index = context.assignments.num_trail_entries();

        // Iterate the trail backwards until a constraint is obtained that propagates at a
        // previous decision level.
        loop {
            trail_index -= 1;

            assert!(is_conflicting(
                &conflicting_hypercube_linear,
                context.assignments.num_trail_entries(),
                context.assignments,
            ));

            let top_of_trail = context.assignments.get_trail_entry(trail_index);
            assert!(
                top_of_trail.predicate.is_lower_bound_predicate()
                    || top_of_trail.predicate.is_upper_bound_predicate()
            );

            let Some((reason_ref, _)) = top_of_trail.reason else {
                let backjump_level = context.assignments.get_decision_level() - 1;
                self.restore_solver(context, backjump_level, conflicting_hypercube_linear);

                return true;
            };

            let reason = hypercube_from_reason(
                &mut HlResolverContext {
                    assignments: context.assignments,
                    reason_store: context.reason_store,
                    propagators: context.propagators,
                },
                reason_ref,
            );
            assert!(
                reason
                    .iter_hypercube()
                    .all(|predicate| context.assignments.is_predicate_satisfied(predicate)),
                "since we do not watch the linear part, reasons must always have satisfied hypercubes"
            );

            match fourier_eliminate(
                &conflicting_hypercube_linear,
                &reason,
                top_of_trail.predicate.get_domain(),
                &mut HlResolverContext {
                    assignments: context.assignments,
                    reason_store: context.reason_store,
                    propagators: context.propagators,
                },
                trail_index,
            ) {
                Some(hypercube_linear) => {
                    trace!("resolving with predicate {:?}", top_of_trail.predicate);
                    conflicting_hypercube_linear = hypercube_linear;
                }
                None => continue,
            }

            // trace!(
            //     "slack after fourier elimination = {}",
            //     slack(
            //         &conflicting_hypercube_linear,
            //         trail_index,
            //         context.assignments
            //     )
            // );

            if !is_conflicting(
                &conflicting_hypercube_linear,
                trail_index,
                context.assignments,
            ) {
                panic!(
                    "Not conflicting. slack_conflict = {}",
                    slack(
                        &conflicting_hypercube_linear,
                        trail_index,
                        context.assignments
                    ),
                );
            }

            // let mut weaken_trail_index = trail_index;
            // let mut weakened_domains = bit_set::BitSet::new();
            // while !is_conflicting(
            //     &conflicting_hypercube_linear,
            //     trail_index,
            //     context.assignments,
            // ) {
            //     weaken_trail_index -= 1;

            //     let weaken_trail_entry = context.assignments.get_trail_entry(weaken_trail_index);

            //     if context
            //         .assignments
            //         .is_initial_bound(weaken_trail_entry.predicate)
            //     {
            //         panic!("now we are weakening wrt initial domain");
            //     }

            //     let domain_index = weaken_trail_entry.predicate.get_domain().id() as usize;
            //     if weakened_domains.contains(domain_index) {
            //         // If we have already seen this domain, then we ignore the predicate since it
            //         // must be weaker than the predicate we saw earlier.
            //         continue;
            //     }

            //     // Weaken wrt the predicates on the trail in reverse order.
            //     if let Some(hypercube_linear) =
            //         conflicting_hypercube_linear.weaken(weaken_trail_entry.predicate)
            //     {
            //         let lhs = evaluate_linear_lower_bound(
            //             &conflicting_hypercube_linear,
            //             trail_index,
            //             context.assignments,
            //         );
            //         trace!("lhs before weakening = {lhs}");

            //         let _ = weakened_domains.insert(domain_index);
            //         trace!("weakening wrt {:?}", weaken_trail_entry.predicate);
            //         trace!("new constraint {hypercube_linear}");
            //         conflicting_hypercube_linear = hypercube_linear;

            //         let lhs = evaluate_linear_lower_bound(
            //             &conflicting_hypercube_linear,
            //             trail_index,
            //             context.assignments,
            //         );
            //         trace!("lhs after weakening = {lhs}");
            //     }
            // }

            // If the conflicting constraint would have propagated at the previous decision level,
            // then we want to stop and backtrack as far as possible.
            let backjump_to =
                determine_backjump(&conflicting_hypercube_linear, context.assignments);

            if let Some(decision_level) = backjump_to
                && decision_level < context.assignments.get_decision_level()
            {
                trace!(
                    "bjdl = {decision_level}, curr_dl = {}",
                    context.assignments.get_decision_level()
                );
                self.restore_solver(context, decision_level, conflicting_hypercube_linear);
                return true;
            }
        }
    }
}

struct HlResolverContext<'a> {
    assignments: &'a Assignments,
    reason_store: &'a mut ReasonStore,
    propagators: &'a PropagatorStore,
}

/// Apply fourier elimination between two hypercube linear constraints.
///
/// This will rewrite b to a tightly-propagating hypercube linear.
///
/// If there is no possible combination, then None is returned.
fn fourier_eliminate(
    constraint_a: &HypercubeLinear,
    constraint_b: &HypercubeLinear,
    domain: DomainId,
    context: &mut HlResolverContext<'_>,
    trail_position: usize,
) -> Option<HypercubeLinear> {
    let domain_weight_in_a = constraint_a.variable_weight(domain)?;
    let domain_weight_in_b = constraint_b.variable_weight(domain)?;

    if domain_weight_in_a.is_positive() == domain_weight_in_b.is_positive() {
        return None;
    }

    let mut tightly_propagating_b =
        tightly_propagating_linear(constraint_b, domain, context, trail_position)?;

    trace!("applying fourier elimination on {domain}",);
    trace!("  - {constraint_a}");
    trace!("  - {constraint_b}");
    trace!("  - tightly propagating: {tightly_propagating_b}");
    trace!(
        "  - slack a: {}",
        slack(constraint_a, trail_position, context.assignments)
    );
    trace!(
        "  - slack b: {}",
        slack(&tightly_propagating_b, trail_position, context.assignments)
    );

    for (_, domain_id) in constraint_b.iter_linear_terms() {
        trace!(
            "{domain_id} in [{}, {}]",
            context
                .assignments
                .get_lower_bound_at_trail_position(domain_id, trail_position),
            context
                .assignments
                .get_upper_bound_at_trail_position(domain_id, trail_position)
        );
    }

    assert_eq!(
        tightly_propagating_b
            .variable_weight(domain)
            .unwrap()
            .abs()
            .get(),
        1,
        "in a tightly-propagating linear, the weight of domain is unit"
    );

    tightly_propagating_b.scale(domain_weight_in_a.abs());

    let mut new_constraint = constraint_a.clone();
    new_constraint.add_polynomial(
        tightly_propagating_b.iter_linear_terms(),
        tightly_propagating_b.linear_rhs(),
    );

    trace!("result = {new_constraint}");
    Some(new_constraint)
}

/// Get the hypercube linear reason for the propagation.
fn hypercube_from_reason(
    context: &mut HlResolverContext<'_>,
    reason: ReasonRef,
) -> HypercubeLinear {
    let propagator_id = context.reason_store.get_propagator(reason);
    let handle = context
        .propagators
        .as_propagator_handle::<HypercubeLinearPropagator>(propagator_id)
        .expect("all propagations by hypercube propagator");

    context
        .propagators
        .get_propagator(handle)
        .expect("we already tested that it is an affine view")
        .hypercube_linear
        .clone()
}

/// Determine the decision level to backjump to. Returns `None` if the constraint does not
/// propagate at a previous decision level.
fn determine_backjump(
    conflicting_hypercube_linear: &HypercubeLinear,
    assignments: &mut Assignments,
) -> Option<usize> {
    for decision_level in (0..=assignments.get_decision_level() - 1).rev() {
        let trail_position = assignments.get_position_at_decision_level(decision_level);
        trace!("testing propagation at dl={decision_level} (tp={trail_position})");

        // If the constraint is not propagating at `decision_level`, then it must have been
        // propagating at `decison_level + 1`, given that the constraint is conflicting (and
        // therefore propagating) at the decision level of the conflict.
        if !is_propagating(conflicting_hypercube_linear, trail_position, assignments) {
            trace!("not propagating {}", decision_level + 1);
            return Some(decision_level + 1);
        }
    }

    None
}

/// Returns true if the given hypercube linear is propagating at the given trail position.
fn is_propagating(
    hypercube_linear: &HypercubeLinear,
    trail_position: usize,
    assignments: &Assignments,
) -> bool {
    // For the hypercube linear to be propagating the hypercube must be satisfied.
    //
    // TODO: In future, propagating can also happen on the hypercube.
    if hypercube_linear.iter_hypercube().any(|predicate| {
        assignments.evaluate_predicate_at_trail_position(predicate, trail_position) != Some(true)
    }) {
        return false;
    }

    // Determine whether an upper bound exceeds what would be propagated.
    let lhs = evaluate_linear_lower_bound(hypercube_linear, trail_position, assignments);

    hypercube_linear.iter_linear_terms().any(|term| {
        let term_lower_bound = term_lower_bound(term, trail_position, assignments);
        let term_upper_bound = term_upper_bound(term, trail_position, assignments);
        let bound = hypercube_linear.linear_rhs() - (lhs - term_lower_bound);

        if term_upper_bound > bound {
            trace!(
                "{hypercube_linear} would propagate [{}{} <= {}]",
                term.0, term.1, bound
            );
            true
        } else {
            false
        }
    })
}

/// Returns true if the given hypercube linear is conflicting at the given trail position.
fn is_conflicting(
    hypercube_linear: &HypercubeLinear,
    trail_position: usize,
    assignments: &Assignments,
) -> bool {
    // For the hypercube linear to be conflicting the hypercube must be satisfied.
    if hypercube_linear.iter_hypercube().any(|predicate| {
        assignments.evaluate_predicate_at_trail_position(predicate, trail_position) != Some(true)
    }) {
        return false;
    }

    slack(hypercube_linear, trail_position, assignments) < 0
}

/// Compute the slack for the linear component of the hypercube linear.
///
/// If the slack is negative, the linear component is conflicting.
fn slack(
    hypercube_linear: &HypercubeLinear,
    trail_position: usize,
    assignments: &Assignments,
) -> i32 {
    let lhs = evaluate_linear_lower_bound(hypercube_linear, trail_position, assignments);
    hypercube_linear.linear_rhs() - lhs
}

fn evaluate_linear_lower_bound(
    hypercube_linear: &HypercubeLinear,
    trail_position: usize,
    assignments: &Assignments,
) -> i32 {
    hypercube_linear
        .iter_linear_terms()
        .map(|term| term_lower_bound(term, trail_position, assignments))
        .sum::<i32>()
}

fn term_lower_bound(
    (weight, domain): (NonZero<i32>, DomainId),
    trail_position: usize,
    assignments: &Assignments,
) -> i32 {
    if weight.is_positive() {
        weight.get() * assignments.get_lower_bound_at_trail_position(domain, trail_position)
    } else {
        weight.get() * assignments.get_upper_bound_at_trail_position(domain, trail_position)
    }
}

fn term_upper_bound(
    (weight, domain): (NonZero<i32>, DomainId),
    trail_position: usize,
    assignments: &Assignments,
) -> i32 {
    if weight.is_positive() {
        weight.get() * assignments.get_upper_bound_at_trail_position(domain, trail_position)
    } else {
        weight.get() * assignments.get_lower_bound_at_trail_position(domain, trail_position)
    }
}

/// Compute a tightly propagating linear component for the specified domain.
///
/// See Section 4.1 of 'Cutting to the Chase. 2013. Jovanovic and de Moura.'
fn tightly_propagating_linear(
    hypercube_linear: &HypercubeLinear,
    domain_to_explain: DomainId,
    context: &mut HlResolverContext<'_>,
    trail_position: usize,
) -> Option<HypercubeLinear> {
    let common_divisor = hypercube_linear.variable_weight(domain_to_explain)?;

    trace!("computing tightly-propagating linear for {hypercube_linear}");
    trace!("common_divisor = {common_divisor}");

    let mut divisible_terms = Vec::with_capacity(hypercube_linear.iter_linear_terms().len());
    divisible_terms.push((common_divisor, domain_to_explain));

    let mut unprocessed_linear = hypercube_linear.clone();
    let _: Vec<_> = unprocessed_linear
        .extract_linear_term_if(move |(_, domain)| domain == domain_to_explain)
        .collect();

    let common_divisor = common_divisor.abs();

    let mut top = trail_position;

    loop {
        // Consume rule.
        divisible_terms.extend(unprocessed_linear.extract_linear_term_if(
            move |(weight, domain)| {
                domain != domain_to_explain && weight.get() % common_divisor.get() == 0
            },
        ));

        // Round (and terminate)
        //
        // Rounding will happen after the loop body.
        if unprocessed_linear.iter_linear_terms().len() == 0 {
            break;
        }

        // Look at the last element in the trail.
        let top_entry = context.assignments.get_trail_entry(top);
        top -= 1;

        // Resolve-implied
        if top_entry.reason.is_some() || context.assignments.is_initial_bound(top_entry.predicate) {
            let reason = if let Some((reason_ref, _)) = top_entry.reason {
                hypercube_from_reason(context, reason_ref)
            } else {
                compute_reason_for_initial_bound(top_entry.predicate)
            };

            if let Some(hypercube_linear) = fourier_eliminate(
                &unprocessed_linear,
                &reason,
                top_entry.predicate.get_domain(),
                context,
                top,
            ) {
                unprocessed_linear = hypercube_linear;
            }

            continue;
        }

        assert!(top_entry.reason.is_none(), "the top entry is a decision");

        let decision_domain = top_entry.predicate.get_domain();

        // This is `c` in the paper.
        let Some(decision_domain_weight) = unprocessed_linear.variable_weight(decision_domain)
        else {
            continue;
        };

        // This is `q` in the paper.
        let mut reason_for_opposite_bound = compute_reason_opposite_decision(context, top_entry);

        match top_entry.predicate.get_predicate_type() {
            crate::predicates::PredicateType::LowerBound => {
                if decision_domain_weight.is_negative() {
                    // Decided-lower-neg

                    reason_for_opposite_bound.scale(decision_domain_weight);

                    unprocessed_linear.add_polynomial(
                        reason_for_opposite_bound.iter_linear_terms(),
                        reason_for_opposite_bound.linear_rhs(),
                    );
                } else {
                    // Decided-lower

                    let k = <i32 as NumExt>::div_ceil(
                        decision_domain_weight.get(),
                        common_divisor.get(),
                    );

                    if let Some(new_weight) = NonZero::new(k * common_divisor.get()) {
                        // ax + as <- ax + as + aky
                        divisible_terms.push((new_weight, decision_domain));
                    }

                    // r <- r + (ak - c)q
                    if let Some(scale) =
                        NonZero::new(common_divisor.get() * k - decision_domain_weight.get())
                    {
                        reason_for_opposite_bound.scale(scale);
                        unprocessed_linear.add_polynomial(
                            reason_for_opposite_bound.iter_linear_terms(),
                            reason_for_opposite_bound.linear_rhs(),
                        );
                    }
                }
            }

            // Decided-upper or Decided-upper-pos
            crate::predicates::PredicateType::UpperBound => {
                if decision_domain_weight.is_positive() {
                    // Decision-Upper-pos
                    reason_for_opposite_bound.scale(decision_domain_weight);

                    unprocessed_linear.add_polynomial(
                        reason_for_opposite_bound.iter_linear_terms(),
                        reason_for_opposite_bound.linear_rhs(),
                    );
                } else {
                    // Decision-upper
                    let k = <i32 as NumExt>::div_floor(
                        decision_domain_weight.get(),
                        common_divisor.get(),
                    );

                    if let Some(new_weight) = NonZero::new(k * common_divisor.get()) {
                        // ax + as <- ax + as + aky
                        divisible_terms.push((new_weight, decision_domain));
                    }

                    // r <- r + (c - ak)q
                    if let Some(scale) =
                        NonZero::new(decision_domain_weight.get() - common_divisor.get() * k)
                    {
                        reason_for_opposite_bound.scale(scale);
                        unprocessed_linear.add_polynomial(
                            reason_for_opposite_bound.iter_linear_terms(),
                            reason_for_opposite_bound.linear_rhs(),
                        );
                    }
                }
            }

            crate::predicates::PredicateType::NotEqual
            | crate::predicates::PredicateType::Equal => {
                todo!("decision cannot be equal or not equal")
            }
        }

        let _ = unprocessed_linear
            .extract_linear_term_if(move |(_, domain)| domain == decision_domain)
            .collect::<Vec<_>>();
    }

    assert_eq!(unprocessed_linear.iter_linear_terms().len(), 0);

    // Perform the rounding that needs to be done
    divisible_terms.iter_mut().for_each(|(weight, _)| {
        *weight = NonZero::new(weight.get() / common_divisor.get()).unwrap()
    });
    let rhs = <i32 as NumExt>::div_floor(unprocessed_linear.linear_rhs(), common_divisor.get());

    Some(HypercubeLinear::new(
        hypercube_linear.iter_hypercube().collect(),
        divisible_terms,
        rhs,
    ))
}

fn compute_reason_opposite_decision(
    context: &mut HlResolverContext<'_>,
    entry: ConstraintProgrammingTrailEntry,
) -> HypercubeLinear {
    assert!(
        entry.reason.is_none(),
        "getting reason opposite from decision"
    );

    let domain = entry.predicate.get_domain();
    let value = entry.predicate.get_right_hand_side();

    let opposite_predicate = match entry.predicate.get_predicate_type() {
        crate::predicates::PredicateType::LowerBound => predicate![domain <= value],
        crate::predicates::PredicateType::UpperBound => predicate![domain >= value],
        crate::predicates::PredicateType::NotEqual | crate::predicates::PredicateType::Equal => {
            todo!("cannot deal with non-bound predicates")
        }
    };

    if context.assignments.is_initial_bound(opposite_predicate) {
        // In case the predicate is an initial bound, the reason is True -> bound.
        compute_reason_for_initial_bound(opposite_predicate)
    } else {
        let opposite_trail_position = context
            .assignments
            .get_trail_position(&opposite_predicate)
            .expect("decisions can only fix to upper or lower bounds");

        let (reason_ref, _) = context
            .assignments
            .get_trail_entry(opposite_trail_position)
            .reason
            .expect("decisions can only fix to upper or lower bounds");

        hypercube_from_reason(context, reason_ref)
    }
}

fn compute_reason_for_initial_bound(predicate: Predicate) -> HypercubeLinear {
    match predicate.get_predicate_type() {
        crate::predicates::PredicateType::LowerBound => HypercubeLinear::new(
            vec![],
            vec![(NonZero::new(-1).unwrap(), predicate.get_domain())],
            -predicate.get_right_hand_side(),
        ),
        crate::predicates::PredicateType::UpperBound => HypercubeLinear::new(
            vec![],
            vec![(NonZero::new(1).unwrap(), predicate.get_domain())],
            predicate.get_right_hand_side(),
        ),
        crate::predicates::PredicateType::NotEqual | crate::predicates::PredicateType::Equal => {
            panic!("decisions cannot be equality or disequality predicates")
        }
    }
}

// Taken from https://docs.rs/num-integer/latest/src/num_integer/lib.rs.html#420-422
#[allow(unused, reason = "experimentation")]
fn gcd(a: i32, b: i32) -> i32 {
    let mut m = a;
    let mut n = b;
    if m == 0 || n == 0 {
        return (m | n).abs();
    }

    // find common factors of 2
    let shift = (m | n).trailing_zeros();

    // The algorithm needs positive numbers, but the minimum value
    // can't be represented as a positive one.
    // It's also a power of two, so the gcd can be
    // calculated by bitshifting in that case

    // Assuming two's complement, the number created by the shift
    // is positive for all numbers except gcd = abs(min value)
    // The call to .abs() causes a panic in debug mode
    if m == i32::MIN || n == i32::MIN {
        let i: i32 = 1 << shift;
        return i.abs();
    }

    // guaranteed to be positive now, rest like unsigned algorithm
    m = m.abs();
    n = n.abs();

    // divide n and m by 2 until odd
    m >>= m.trailing_zeros();
    n >>= n.trailing_zeros();

    while m != n {
        if m > n {
            m -= n;
            m >>= m.trailing_zeros();
        } else {
            n -= m;
            n >>= n.trailing_zeros();
        }
    }
    m << shift
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::containers::StorageKey;
    use crate::engine::TrailedValues;
    use crate::engine::conflict_analysis::SemanticMinimiser;
    use crate::engine::notifications::NotificationEngine;
    use crate::engine::propagation::PropagationContextMut;
    use crate::engine::propagation::Propagator;
    use crate::proof::InferenceCode;

    #[test]
    fn tightly_propagating_example_paper() {
        let mut assignments = Assignments::default();
        let x = assignments.grow(-100, 100);
        let y = assignments.grow(0, 100);
        let z = assignments.grow(-100, 100);

        let mut reason_store = ReasonStore::default();
        let mut notification_engine = NotificationEngine::default();
        notification_engine.grow();
        notification_engine.grow();
        notification_engine.grow();

        let mut propagators = PropagatorStore::default();
        let i2 = propagators
            .new_propagator()
            .populate(HypercubeLinearPropagator::new(
                HypercubeLinear::new(vec![], vec![(NonZero::new(-1).unwrap(), x)], -2),
                InferenceCode::create_from_index(0),
            ));
        let i3 = propagators
            .new_propagator()
            .populate(HypercubeLinearPropagator::new(
                HypercubeLinear::new(
                    vec![],
                    vec![
                        (NonZero::new(-1).unwrap(), y),
                        (NonZero::new(1).unwrap(), x),
                    ],
                    -7,
                ),
                InferenceCode::create_from_index(0),
            ));
        let i4 = propagators
            .new_propagator()
            .populate(HypercubeLinearPropagator::new(
                HypercubeLinear::new(
                    vec![],
                    vec![
                        (NonZero::new(-3).unwrap(), z),
                        (NonZero::new(2).unwrap(), y),
                        (NonZero::new(-5).unwrap(), x),
                    ],
                    0,
                ),
                InferenceCode::create_from_index(0),
            ));

        propagators
            .get_propagator(i2)
            .unwrap()
            .debug_propagate_from_scratch(PropagationContextMut::new(
                &mut TrailedValues::default(),
                &mut assignments,
                &mut reason_store,
                &mut SemanticMinimiser::default(),
                &mut notification_engine,
                i2.propagator_id(),
            ))
            .expect("no empty domain");

        propagators
            .get_propagator(i3)
            .unwrap()
            .debug_propagate_from_scratch(PropagationContextMut::new(
                &mut TrailedValues::default(),
                &mut assignments,
                &mut reason_store,
                &mut SemanticMinimiser::default(),
                &mut notification_engine,
                i3.propagator_id(),
            ))
            .expect("no empty domain");

        let _ = assignments
            .post_predicate(predicate![x <= 2], None, &mut notification_engine)
            .expect("no empty domain");

        propagators
            .get_propagator(i4)
            .unwrap()
            .debug_propagate_from_scratch(PropagationContextMut::new(
                &mut TrailedValues::default(),
                &mut assignments,
                &mut reason_store,
                &mut SemanticMinimiser::default(),
                &mut notification_engine,
                i4.propagator_id(),
            ))
            .expect("no empty domain");

        let trail_position = assignments.get_trail_position(&predicate![z >= 3]).unwrap();

        let reason = tightly_propagating_linear(
            &propagators.get_propagator(i4).unwrap().hypercube_linear,
            z,
            &mut HlResolverContext {
                assignments: &assignments,
                reason_store: &mut reason_store,
                propagators: &propagators,
            },
            trail_position,
        )
        .unwrap();

        let expected = HypercubeLinear::new(
            vec![],
            vec![
                (NonZero::new(-1).unwrap(), z),
                (NonZero::new(-2).unwrap(), x),
            ],
            -7,
        );

        assert_eq!(reason, expected);
    }
}
