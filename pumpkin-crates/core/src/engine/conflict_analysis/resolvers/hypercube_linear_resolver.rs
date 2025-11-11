use std::num::NonZero;

use log::debug;
use log::trace;

use crate::basic_types::StoredConflictInfo;
use crate::basic_types::moving_averages::CumulativeMovingAverage;
use crate::basic_types::moving_averages::MovingAverage;
use crate::create_statistics_struct;
use crate::engine::Assignments;
use crate::engine::conflict_analysis::ConflictAnalysisContext;
use crate::engine::conflict_analysis::ConflictResolver;
use crate::engine::propagation::constructor::PropagatorConstructor;
use crate::engine::propagation::constructor::PropagatorConstructorContext;
use crate::engine::propagation::store::PropagatorStore;
use crate::engine::reason::ReasonRef;
use crate::engine::reason::ReasonStore;
use crate::predicate;
use crate::predicates::Predicate;
use crate::predicates::PredicateType;
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
            conflict_trigger,
            ..
        } = context.solver_state.get_conflict_info()
        else {
            panic!(
                "Cannot start hypercube analysis from {:?}",
                context.solver_state.get_conflict_info()
            );
        };

        trace!("conflicting predicate = {}", conflict_trigger);

        let propagator_id = context.reason_store.get_propagator(conflict_trigger_reason);
        let handle = context
            .propagators
            .as_propagator_handle::<HypercubeLinearPropagator>(propagator_id)
            .expect("all propagations by hypercube propagator");

        let propagator = context
            .propagators
            .get_propagator(handle)
            .expect("we already tested that it is a hypercube linear");

        trace!("constraint ID = {:?}", propagator.constraint_tag);

        propagator.hypercube_linear.clone()
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

        panic!("stop");

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

    fn learn_hypercube_linear(
        &mut self,
        context: HlResolverContext,
        mut conflicting_hypercube_linear: HypercubeLinear,
    ) -> (usize, HypercubeLinear) {
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
                panic!("resolution should never reach the last decision");
            };

            trace!("processing {}", top_of_trail.predicate);

            let reason = hypercube_from_reason(
                &mut HlResolverContext {
                    assignments: context.assignments,
                    reason_store: context.reason_store,
                    propagators: context.propagators,
                },
                reason_ref,
            );

            trace!("reason = {}", top_of_trail.predicate);

            if hypercube_models_bound(&conflicting_hypercube_linear, top_of_trail.predicate) {
                // Here we need to perform propositional resolution to eliminate the
                // variable from the hypercube.

                conflicting_hypercube_linear = propositional_resolution(
                    context.assignments,
                    trail_index,
                    conflicting_hypercube_linear,
                    &reason,
                    top_of_trail.predicate,
                );
            } else {
                trace!("no propositional resolution");
            }

            match fourier_eliminate(
                context.assignments,
                trail_index,
                &conflicting_hypercube_linear,
                &reason,
                top_of_trail.predicate,
            ) {
                Some(hypercube_linear) => {
                    trace!(
                        "fourier elimination with predicate {:?}",
                        top_of_trail.predicate
                    );
                    conflicting_hypercube_linear = hypercube_linear;
                }
                None => {
                    trace!("no fourier resolution");
                    continue;
                }
            }

            if !is_conflicting(
                &conflicting_hypercube_linear,
                trail_index,
                context.assignments,
            ) {
                panic!(
                    "Not conflicting. slack_conflict = {}",
                    compute_slack(
                        &conflicting_hypercube_linear,
                        trail_index,
                        context.assignments
                    ),
                );
            }

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

                return (decision_level, conflicting_hypercube_linear);
            }
        }
    }
}

impl ConflictResolver for HypercubeLinearResolver {
    fn log_statistics(&self, statistic_logger: crate::statistics::StatisticLogger) {
        self.statistics.log(statistic_logger);
    }

    fn resolve_conflict(&mut self, mut context: ConflictAnalysisContext) -> bool {
        let conflicting_hypercube_linear = self.compute_conflicting_hypercube_linear(&mut context);
        let (backjump_level, learned_hypercube_linear) = self.learn_hypercube_linear(
            HlResolverContext {
                assignments: context.assignments,
                reason_store: context.reason_store,
                propagators: context.propagators,
            },
            conflicting_hypercube_linear,
        );
        self.restore_solver(context, backjump_level, learned_hypercube_linear);
        true
    }
}

fn propositional_resolution(
    assignments: &Assignments,
    trail_position: usize,
    conflicting_hypercube_linear: HypercubeLinear,
    reason: &HypercubeLinear,
    propagated_predicate: Predicate,
) -> HypercubeLinear {
    let weakened_reason =
        weaken_to_clause(assignments, trail_position, reason, propagated_predicate);

    let hypercube = conflicting_hypercube_linear
        .iter_hypercube()
        .chain(weakened_reason.iter_hypercube())
        .filter(|predicate| {
            // Only keep predicates that are not the propagated predicate or its
            // opposite.
            !predicate.implies(propagated_predicate) && !predicate.implies(!propagated_predicate)
        })
        .collect();

    HypercubeLinear::new(
        hypercube,
        conflicting_hypercube_linear.iter_linear_terms().collect(),
        conflicting_hypercube_linear.linear_rhs(),
    )
}

fn weaken_to_clause(
    assignments: &Assignments,
    trail_position: usize,
    hypercube_linear: &HypercubeLinear,
    propagated_predicate: Predicate,
) -> HypercubeLinear {
    let hypercube = hypercube_linear
        .iter_hypercube()
        .chain(
            hypercube_linear
                .iter_linear_terms()
                .map(|(weight, domain)| {
                    let predicate = if weight.is_positive() {
                        let lower_bound =
                            assignments.get_lower_bound_at_trail_position(domain, trail_position);
                        predicate![domain >= lower_bound]
                    } else {
                        let upper_bound =
                            assignments.get_upper_bound_at_trail_position(domain, trail_position);
                        predicate![domain <= upper_bound]
                    };

                    // If the current predicate is the propagated one, then we want its
                    // opposite in the final clause.
                    if propagated_predicate.implies(predicate) {
                        !predicate
                    } else {
                        predicate
                    }
                }),
        )
        .collect();

    HypercubeLinear::new(hypercube, vec![], -1)
}

fn hypercube_models_bound(hypercube_linear: &HypercubeLinear, predicate: Predicate) -> bool {
    hypercube_linear
        .iter_hypercube()
        .any(|hypercube_predicate| hypercube_predicate.implies(predicate))
}

struct HlResolverContext<'a> {
    assignments: &'a Assignments,
    reason_store: &'a mut ReasonStore,
    propagators: &'a PropagatorStore,
}

/// Apply fourier elimination between two hypercube linear constraints.
///
/// If there is no possible combination, then None is returned.
fn fourier_eliminate(
    assignments: &Assignments,
    trail_position: usize,
    constraint_a: &HypercubeLinear,
    constraint_b: &HypercubeLinear,
    pivot_predicate: Predicate,
) -> Option<HypercubeLinear> {
    let domain_weight_in_a = constraint_a.variable_weight(pivot_predicate.get_domain())?;
    let domain_weight_in_b = constraint_b.variable_weight(pivot_predicate.get_domain())?;

    if domain_weight_in_a.is_positive() == domain_weight_in_b.is_positive() {
        return None;
    }

    let tightly_propagating_b =
        tightly_propagating_linear(assignments, trail_position, constraint_b, pivot_predicate)?;

    trace!("applying fourier elimination on {pivot_predicate}",);
    trace!("  - {constraint_a}");
    trace!("  - {constraint_b}");
    trace!("  - tightly propagating: {tightly_propagating_b}");
    trace!(
        "  - slack a: {}",
        compute_slack(constraint_a, trail_position, assignments)
    );
    trace!(
        "  - slack b: {}",
        compute_slack(&tightly_propagating_b, trail_position, assignments)
    );

    let new_hypercube = constraint_a
        .iter_hypercube()
        .chain(tightly_propagating_b.iter_hypercube())
        .collect();

    let linear_terms = constraint_a
        .iter_linear_terms()
        .filter(|(_, domain)| *domain != pivot_predicate.get_domain())
        .collect();

    let new_rhs =
        constraint_a.linear_rhs() + domain_weight_in_a.get() * tightly_propagating_b.linear_rhs();

    let new_constraint = HypercubeLinear::new(new_hypercube, linear_terms, new_rhs);

    trace!("result = {new_constraint}");
    Some(new_constraint)
}

/// Compute a tightly propagating linear component for the specified domain.
///
/// A linear component is tightly propagating if and only if the slack is equal to 0.
fn tightly_propagating_linear(
    assignments: &Assignments,
    trail_position: usize,
    hypercube_linear: &HypercubeLinear,
    predicate_as_linear: Predicate,
) -> Option<HypercubeLinear> {
    weaken_to_clause_except(
        assignments,
        trail_position,
        hypercube_linear,
        predicate_as_linear,
    )
}

fn weaken_to_clause_except(
    assignments: &Assignments,
    trail_position: usize,
    hypercube_linear: &HypercubeLinear,
    predicate_as_linear: Predicate,
) -> Option<HypercubeLinear> {
    let can_propagate_predicate = hypercube_linear
        .iter_linear_terms()
        .find(|(_, domain)| *domain == predicate_as_linear.get_domain())
        .is_some_and(|(weight, _)| {
            predicate_as_linear.get_predicate_type()
                == if weight.is_positive() {
                    PredicateType::UpperBound
                } else {
                    PredicateType::LowerBound
                }
        });
    if !can_propagate_predicate {
        return None;
    }

    let hypercube = hypercube_linear
        .iter_hypercube()
        .chain(
            hypercube_linear
                .iter_linear_terms()
                .filter(|(_, domain)| *domain != predicate_as_linear.get_domain())
                .map(|(weight, domain)| {
                    if weight.is_positive() {
                        let lower_bound =
                            assignments.get_lower_bound_at_trail_position(domain, trail_position);
                        predicate![domain >= lower_bound]
                    } else {
                        let upper_bound =
                            assignments.get_upper_bound_at_trail_position(domain, trail_position);
                        predicate![domain <= upper_bound]
                    }
                }),
        )
        .collect();

    // Convert the predicate as linear to a valid linear inequality. This means multiplying by
    // -1 in case the predicate is a lower-bound predicate.
    let (terms, rhs) = match predicate_as_linear.get_predicate_type() {
        PredicateType::LowerBound => (
            vec![(NonZero::new(-1).unwrap(), predicate_as_linear.get_domain())],
            -predicate_as_linear.get_right_hand_side(),
        ),
        PredicateType::UpperBound => (
            vec![(NonZero::new(1).unwrap(), predicate_as_linear.get_domain())],
            predicate_as_linear.get_right_hand_side(),
        ),
        PredicateType::NotEqual | PredicateType::Equal => {
            panic!("only bounds predicates are expected")
        }
    };

    Some(HypercubeLinear::new(hypercube, terms, rhs))
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
    assignments: &Assignments,
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
    let satisfied_bounds_in_hypercube = hypercube_linear
        .iter_hypercube()
        .filter(|&predicate| {
            assignments.evaluate_predicate_at_trail_position(predicate, trail_position)
                == Some(true)
        })
        .count();

    let slack = compute_slack(hypercube_linear, trail_position, assignments);

    // The linear term is conflicting and exactly one bounds constraint is unassigned when the
    // rest are true. That means that bounds constraint can be propagated to false.
    if satisfied_bounds_in_hypercube == hypercube_linear.iter_hypercube().len() - 1 && slack < 0 {
        trace!("would propagate hypercube");
        return true;
    }

    // All bounds in the hypercube are satisfied, meaning the linear term _could_ propagate.
    if satisfied_bounds_in_hypercube == hypercube_linear.iter_hypercube().len() {
        // Determine whether an upper bound exceeds what would be propagated.
        return hypercube_linear.iter_linear_terms().any(|term| {
            let term_lower_bound = term_lower_bound(term, trail_position, assignments);
            let term_upper_bound = term_upper_bound(term, trail_position, assignments);
            let bound = slack + term_lower_bound;

            if term_upper_bound > bound {
                trace!(
                    "{hypercube_linear} would propagate [{}{} <= {}]",
                    term.0, term.1, bound
                );
                true
            } else {
                false
            }
        });
    }

    false
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

    compute_slack(hypercube_linear, trail_position, assignments) < 0
}

/// Compute the slack for the linear component of the hypercube linear.
///
/// If the slack is negative, the linear component is conflicting.
fn compute_slack(
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
    use crate::{
        containers::KeyGenerator,
        engine::{
            notifications::NotificationEngine,
            reason::{Reason, StoredReason},
        },
    };

    use super::*;

    #[test]
    fn tightly_propagating_b_for_positive_weight() {
        let mut assignments = Assignments::default();
        let x = assignments.grow(2, 10);
        let y = assignments.grow(0, 10);

        let constraint = HypercubeLinear::new(
            vec![predicate![x >= 2]],
            vec![(NonZero::new(-1).unwrap(), y)],
            -5,
        );

        let tightly_propagating = tightly_propagating_linear(
            &assignments,
            assignments.num_trail_entries(),
            &constraint,
            predicate![y >= 5],
        )
        .unwrap();

        assert_eq!(constraint, tightly_propagating);
    }

    #[test]
    fn tightly_propagating_b_for_negative_weight() {
        let mut assignments = Assignments::default();
        let x = assignments.grow(2, 10);
        let y = assignments.grow(0, 10);

        let constraint = HypercubeLinear::new(
            vec![predicate![x >= 2]],
            vec![(NonZero::new(1).unwrap(), y)],
            5,
        );

        let tightly_propagating = tightly_propagating_linear(
            &assignments,
            assignments.num_trail_entries(),
            &constraint,
            predicate![y <= 5],
        )
        .unwrap();

        assert_eq!(constraint, tightly_propagating);
    }

    #[test]
    fn resolution_test() {
        env_logger::init();

        let mut constraint_tags = KeyGenerator::default();
        let mut inference_codes = KeyGenerator::default();

        let mut notification_engine = NotificationEngine::default();
        notification_engine.grow();
        notification_engine.grow();
        notification_engine.grow();

        let mut assignments = Assignments::default();
        let x = assignments.grow(2, 10);
        let y = assignments.grow(0, 10);
        let z = assignments.grow(0, 10);

        let mut reason_store = ReasonStore::default();
        let mut propagators = PropagatorStore::default();

        let inf_a = inference_codes.next_key();
        let c_a = propagators
            .new_propagator()
            .populate(HypercubeLinearPropagator::new(
                HypercubeLinear::new(
                    vec![predicate![x >= 1]],
                    vec![(NonZero::new(1).unwrap(), y), (NonZero::new(1).unwrap(), z)],
                    5,
                ),
                constraint_tags.next_key(),
                inf_a,
            ));

        let c_b = propagators
            .new_propagator()
            .populate(HypercubeLinearPropagator::new(
                HypercubeLinear::new(
                    vec![predicate![y >= 1]],
                    vec![(NonZero::new(-1).unwrap(), z)],
                    -2,
                ),
                constraint_tags.next_key(),
                inference_codes.next_key(),
            ));

        assignments.increase_decision_level();
        reason_store.increase_decision_level();
        let _ = assignments
            .post_predicate(predicate![x >= 1], None, &mut notification_engine)
            .unwrap();
        assignments.increase_decision_level();
        reason_store.increase_decision_level();
        let _ = assignments
            .post_predicate(predicate![y >= 4], None, &mut notification_engine)
            .unwrap();

        let reason_ref = reason_store.push(c_a.propagator_id(), StoredReason::DynamicLazy(0));
        let _ = assignments
            .post_predicate(
                predicate![z <= 1],
                Some((reason_ref, inf_a)),
                &mut notification_engine,
            )
            .unwrap();

        let mut resolver = HypercubeLinearResolver::default();
        let conflicting_constraint = propagators
            .get_propagator(c_b)
            .unwrap()
            .hypercube_linear
            .clone();

        let (backjump_level, learned_constraint) = resolver.learn_hypercube_linear(
            HlResolverContext {
                assignments: &assignments,
                reason_store: &mut reason_store,
                propagators: &propagators,
            },
            conflicting_constraint,
        );

        assert_eq!(backjump_level, 1);

        let expected_constraint =
            HypercubeLinear::new(vec![predicate![x >= 1], predicate![y >= 4]], vec![], -1);

        assert_eq!(learned_constraint, expected_constraint);
    }
}
