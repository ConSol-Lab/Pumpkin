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
use crate::propagators::HypercubeLinear;
use crate::propagators::HypercubeLinearPropagator;
use crate::propagators::HypercubeLinearPropagatorArgs;
use crate::statistics::Statistic;
use crate::variables::DomainId;
use crate::variables::IntegerVariable;
use crate::variables::TransformableVariable;

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
    ) -> (HypercubeLinear, Vec<Predicate>) {
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

        let mut reason_set = explain_initial_conflict(
            &propagator.hypercube_linear,
            context.assignments.num_trail_entries() - 1,
            context.assignments,
            conflict_trigger,
        );

        reason_set.push(!conflict_trigger);

        trace!(
            "constraint ID = {:?}, pid = {}",
            propagator.constraint_tag, propagator_id
        );
        trace!("learned = {:?}", propagator.is_learned);

        (propagator.hypercube_linear.clone(), reason_set)
    }

    fn restore_solver(
        &mut self,
        context: &mut ConflictAnalysisContext<'_>,
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
            is_learned: true,
        }
        .create(constructor_context);

        let new_propagator_id = propagator_slot.populate(propagator);
        trace!(
            "New constraint tag = {constraint_tag:?} (pid = {})",
            new_propagator_id.propagator_id()
        );
        context
            .propagator_queue
            .enqueue_propagator(new_propagator_id.propagator_id(), 0);
    }

    fn learn_hypercube_linear(
        &mut self,
        context: HlResolverContext,
        mut conflicting_hypercube_linear: HypercubeLinear,
        conflicting_reason_set: Vec<Predicate>,
    ) -> (HypercubeLinear, Vec<Predicate>) {
        debug!(
            "Resolving conflict (dl = {}) {conflicting_hypercube_linear}",
            context.assignments.get_decision_level()
        );

        let mut conflicting_reason_set = HypercubeLinear::clause(conflicting_reason_set);

        let conflict_dl = context.assignments.get_decision_level();
        let mut trail_index = context.assignments.num_trail_entries();

        // Iterate the trail backwards until a constraint is obtained that propagates at a
        // previous decision level.
        loop {
            // println!("{conflicting_hypercube_linear}");
            // dbg!(&conflicting_reason_set);

            // for p in conflicting_reason_set.iter_hypercube() {
            //     let domain = p.get_domain();
            //     println!(
            //         "{} in [{}, {}]",
            //         domain,
            //         domain.lower_bound_at_trail_position(context.assignments, trail_index),
            //         domain.upper_bound_at_trail_position(context.assignments, trail_index)
            //     );
            // }

            assert!(conflicting_reason_set.iter_hypercube().all(|predicate| {
                let truth_value = context
                    .assignments
                    .evaluate_predicate_at_trail_position(predicate, trail_index);

                // let domain = predicate.get_domain();
                // println!(
                //     "{predicate} -> {} in [{}, {}]",
                //     domain,
                //     domain.lower_bound_at_trail_position(context.assignments, trail_index),
                //     domain.upper_bound_at_trail_position(context.assignments, trail_index)
                // );

                truth_value == Some(true)
            }));

            let reasons_on_current_dl = conflicting_reason_set
                .iter_hypercube()
                .filter(|predicate| {
                    let predicate_dl = context
                        .assignments
                        .get_decision_level_for_predicate(predicate)
                        .unwrap();

                    // println!("{predicate}: {predicate_dl}");

                    predicate_dl == conflict_dl
                })
                .collect::<Vec<_>>();

            trace!(
                "reasons on conflict dl = {reasons_on_current_dl:?} (out of {})",
                conflicting_reason_set.iter_hypercube().len()
            );

            if reasons_on_current_dl.len() <= 1 {
                return (
                    conflicting_hypercube_linear,
                    conflicting_reason_set.iter_hypercube().collect(),
                );
            }

            assert_eq!(
                is_conflicting(
                    &conflicting_hypercube_linear,
                    trail_index,
                    context.assignments,
                ),
                ConflictingStatus::Conflicting
            );

            trail_index -= 1;
            let top_of_trail = context.assignments.get_trail_entry(trail_index);

            assert!(
                top_of_trail.predicate.is_lower_bound_predicate()
                    || top_of_trail.predicate.is_upper_bound_predicate()
            );

            // Remove the top of trail from the reason set if it is in the reason set.
            if !conflicting_reason_set
                .iter_hypercube()
                .any(|p| p == top_of_trail.predicate)
            {
                // The current top of trail is not in the reason set and therefore does not
                // contribute to the conflict.
                continue;
            }

            let pivot_predicate = top_of_trail.predicate;

            trace!("processing {}", pivot_predicate);

            let Some((reason_ref, _)) = top_of_trail.reason else {
                panic!("resolution should never reach the last decision");
            };

            let reason = hypercube_from_reason(
                &mut HlResolverContext {
                    assignments: context.assignments,
                    reason_store: context.reason_store,
                    propagators: context.propagators,
                },
                reason_ref,
            );

            let mut elimination_happened = false;

            match fourier_eliminate(
                context.assignments,
                trail_index,
                &conflicting_hypercube_linear,
                &reason,
                top_of_trail.predicate,
            ) {
                Ok(hypercube_linear) => {
                    let new_slack =
                        compute_slack(&hypercube_linear, trail_index, context.assignments);
                    trace!("new slack = {new_slack}");

                    let new_conflicting = if new_slack >= 0 {
                        let weakened_conflicting =
                            conflicting_hypercube_linear.weaken(pivot_predicate).expect("if we could do fourier elimination then the top of trail contributes to the conflict and can therefore be weakened on");
                        let weakened_conflicting_slack =
                            compute_slack(&weakened_conflicting, trail_index, context.assignments);
                        assert!(
                            weakened_conflicting_slack < 0,
                            "weakening like this does not affect slack"
                        );

                        propositional_resolution(
                            context.assignments,
                            trail_index,
                            weakened_conflicting,
                            &reason,
                            top_of_trail.predicate,
                        )
                    } else {
                        hypercube_linear
                    };

                    conflicting_hypercube_linear = new_conflicting;
                    elimination_happened = true;
                }
                Err(FourierError::ResultOfEliminationTriviallySatisfiable) => {
                    let weakened_conflicting =
                            conflicting_hypercube_linear.weaken(pivot_predicate).expect("if we could do fourier elimination then the top of trail contributes to the conflict and can therefore be weakened on");
                    let weakened_conflicting_slack =
                        compute_slack(&weakened_conflicting, trail_index, context.assignments);
                    assert!(
                        weakened_conflicting_slack < 0,
                        "weakening like this does not affect slack"
                    );

                    conflicting_hypercube_linear = propositional_resolution(
                        context.assignments,
                        trail_index,
                        weakened_conflicting,
                        &reason,
                        top_of_trail.predicate,
                    );

                    elimination_happened = true;
                }
                Err(FourierError::NoVariableElimination) => {
                    trace!("no fourier resolution");
                }
            }

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
                elimination_happened = true;
                let new_slack = compute_slack(
                    &conflicting_hypercube_linear,
                    trail_index,
                    context.assignments,
                );
                trace!("slack after resh = {new_slack}");
            } else {
                trace!("no propositional resolution");
            }

            assert!(
                elimination_happened,
                "if the predicate contributes to the conflict, an elimination should be possible"
            );

            // In this case the fourier elimination may have removed multiple variables, which we also need to remove from the new reason set.
            conflicting_reason_set = explain_conflict(
                &conflicting_hypercube_linear,
                trail_index,
                context.assignments,
            );
        }
    }
}

impl ConflictResolver for HypercubeLinearResolver {
    fn log_statistics(&self, statistic_logger: crate::statistics::StatisticLogger) {
        self.statistics.log(statistic_logger);
    }

    fn resolve_conflict(&mut self, mut context: ConflictAnalysisContext) -> bool {
        let (conflicting_hypercube_linear, reason_set) =
            self.compute_conflicting_hypercube_linear(&mut context);

        let (learned_hypercube_linear, reason_set) = self.learn_hypercube_linear(
            HlResolverContext {
                assignments: context.assignments,
                reason_store: context.reason_store,
                propagators: context.propagators,
            },
            conflicting_hypercube_linear.clone(),
            reason_set,
        );

        assert_ne!(
            learned_hypercube_linear, conflicting_hypercube_linear,
            "learning should produce a new constraint"
        );

        let backjump_level = reason_set
            .iter()
            .map(|p| {
                context
                    .assignments
                    .get_decision_level_for_predicate(p)
                    .expect("all predicates are assigned (and true)")
            })
            .filter(|&dl| dl < context.assignments.get_decision_level())
            .max()
            .unwrap_or(0);

        self.restore_solver(&mut context, backjump_level, learned_hypercube_linear);

        true
    }
}

fn explain_initial_conflict(
    hypercube_linear: &HypercubeLinear,
    trail_position: usize,
    assignments: &Assignments,
    conflict_trigger: Predicate,
) -> Vec<Predicate> {
    hypercube_linear
        .iter_hypercube()
        .chain(
            hypercube_linear
                .iter_linear_terms()
                .filter_map(|(weight, domain)| {
                    let term = domain.scaled(weight.get());
                    let term_lower_bound =
                        term.lower_bound_at_trail_position(assignments, trail_position);
                    let predicate = predicate![term >= term_lower_bound];

                    if conflict_trigger.implies(predicate) {
                        None
                    } else {
                        Some(predicate)
                    }
                }),
        )
        .collect()
}

fn explain_conflict(
    hypercube_linear: &HypercubeLinear,
    trail_position: usize,
    assignments: &Assignments,
) -> HypercubeLinear {
    let hypercube = hypercube_linear
        .iter_hypercube()
        .chain(
            hypercube_linear
                .iter_linear_terms()
                .map(|(weight, domain)| {
                    let term = domain.scaled(weight.get());
                    let term_lower_bound =
                        term.lower_bound_at_trail_position(assignments, trail_position);
                    predicate![term >= term_lower_bound]
                }),
        )
        .collect();

    HypercubeLinear::clause(hypercube)
}

fn propositional_resolution(
    assignments: &Assignments,
    trail_position: usize,
    conflicting_hypercube_linear: HypercubeLinear,
    reason: &HypercubeLinear,
    pivot_predicate: Predicate,
) -> HypercubeLinear {
    trace!("applying propositional resolution on {pivot_predicate}",);
    trace!("  - {conflicting_hypercube_linear}");
    trace!("  - {reason}");

    let weakened_reason = weaken_to_clause(assignments, trail_position, reason, pivot_predicate);
    trace!("     - as clause: {weakened_reason}");

    let hypercube = conflicting_hypercube_linear
        .iter_hypercube()
        .chain(weakened_reason.iter_hypercube())
        .filter(|predicate| {
            // Only keep predicates that are not the propagated predicate or its
            // opposite.
            !predicate.implies(pivot_predicate) && !predicate.implies(!pivot_predicate)
        })
        .filter(|predicate| {
            // Ignore predicates that are true at the root.
            assignments
                .get_decision_level_for_predicate(predicate)
                .unwrap()
                > 0
        })
        .collect();

    let mut new_constraint = HypercubeLinear::new(
        hypercube,
        conflicting_hypercube_linear.iter_linear_terms().collect(),
        conflicting_hypercube_linear.linear_rhs(),
    )
    .expect("not trivially satisfiable");

    // The bounds in the explanation may contribute to the slack of the constraint.
    // After backtracking that would result in a possibly non-negative slack.
    // Therefore, weaken on these bounds just in case.
    for predicate in weakened_reason
        .iter_hypercube()
        .filter(|&predicate| predicate != !pivot_predicate)
    {
        if let Some(l) = new_constraint.weaken(predicate) {
            new_constraint = l;
        }
    }

    trace!("result = {new_constraint}");

    new_constraint
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
                .filter_map(|(weight, domain)| {
                    if domain == propagated_predicate.get_domain() {
                        return None;
                    }

                    let term = domain.scaled(weight.get());
                    let term_lower_bound =
                        term.lower_bound_at_trail_position(assignments, trail_position);

                    let predicate_in_clause = predicate![term >= term_lower_bound];
                    Some(predicate_in_clause)
                }),
        )
        .chain(std::iter::once(!propagated_predicate))
        .collect::<Vec<_>>();

    HypercubeLinear::clause(hypercube)
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

enum FourierError {
    NoVariableElimination,
    ResultOfEliminationTriviallySatisfiable,
}

/// Apply fourier elimination between two hypercube linear constraints.
///
/// If there is no possible combination, then None is returned.
fn fourier_eliminate(
    assignments: &Assignments,
    trail_position: usize,
    conflicting_hypercube_linear: &HypercubeLinear,
    reason: &HypercubeLinear,
    pivot_predicate: Predicate,
) -> Result<HypercubeLinear, FourierError> {
    let maybe_weight_in_conflicting =
        conflicting_hypercube_linear.variable_weight(pivot_predicate.get_domain());
    let maybe_weight_in_reason = reason.variable_weight(pivot_predicate.get_domain());

    let (weight_in_conflicting, weight_in_reason) =
        match (maybe_weight_in_conflicting, maybe_weight_in_reason) {
            (Some(a_weight), Some(b_weight)) if a_weight.get() * b_weight.get() < 0 => {
                (a_weight, b_weight)
            }

            // Either the domain is not in one of the two constraints, or they don't have
            // opposing signs. In both cases, we cannot perform fourier
            // elimination on the specified domain.
            _ => return Err(FourierError::NoVariableElimination),
        };

    trace!("applying fourier elimination on {pivot_predicate}",);
    trace!("  - {conflicting_hypercube_linear}");
    trace!("  - {reason}");
    trace!(
        "  - slack a: {}",
        compute_slack(conflicting_hypercube_linear, trail_position, assignments)
    );
    trace!(
        "  - slack b: {}",
        compute_slack(reason, trail_position, assignments)
    );

    // Determine by how much to scale both linear terms.
    let g = gcd(
        weight_in_conflicting.get().abs(),
        weight_in_reason.get().abs(),
    );
    // We have to remake the non-zero as there is no API for division on NonZero yet.
    let scale_conflicting = NonZero::new(weight_in_reason.abs().get() / g).unwrap();
    let scale_reason = NonZero::new(weight_in_conflicting.abs().get() / g).unwrap();

    // The linear terms of the new hypercube linear is the addition of the scaled terms of both
    // input constraints.
    let linear_terms = conflicting_hypercube_linear
        .iter_linear_terms()
        .map(|(weight, domain)| (weight.checked_mul(scale_conflicting).unwrap(), domain))
        .chain(
            reason
                .iter_linear_terms()
                .map(|(weight, domain)| (weight.checked_mul(scale_reason).unwrap(), domain)),
        )
        .collect();

    let hypercube = conflicting_hypercube_linear
        .iter_hypercube()
        .chain(reason.iter_hypercube())
        .collect();

    let linear_rhs = conflicting_hypercube_linear
        .linear_rhs()
        .checked_mul(scale_conflicting.get())
        .unwrap()
        + reason.linear_rhs().checked_mul(scale_reason.get()).unwrap();

    let mut new_constraint = HypercubeLinear::new(hypercube, linear_terms, linear_rhs)
        .ok_or(FourierError::ResultOfEliminationTriviallySatisfiable)?;

    for bound in reason.iter_hypercube() {
        if let Some(l) = new_constraint.weaken(bound) {
            new_constraint = l;
        }
    }

    trace!("result = {new_constraint}");
    Ok(new_constraint)
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

    let propagator = context
        .propagators
        .get_propagator(handle)
        .expect("we already tested that it is an affine view");

    trace!(
        "combining with constraint ID {:?} (is_learned = {})",
        propagator.constraint_tag, propagator.is_learned
    );

    propagator.hypercube_linear.clone()
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum ConflictingStatus {
    PremisesNotTrue,
    NonNegativeSlack,
    Conflicting,
}

/// Returns true if the given hypercube linear is conflicting at the given trail position.
fn is_conflicting(
    hypercube_linear: &HypercubeLinear,
    trail_position: usize,
    assignments: &Assignments,
) -> ConflictingStatus {
    // For the hypercube linear to be conflicting the hypercube must be satisfied.
    if hypercube_linear.iter_hypercube().any(|predicate| {
        assignments.evaluate_predicate_at_trail_position(predicate, trail_position) != Some(true)
    }) {
        return ConflictingStatus::PremisesNotTrue;
    }

    if compute_slack(hypercube_linear, trail_position, assignments) < 0 {
        ConflictingStatus::Conflicting
    } else {
        ConflictingStatus::NonNegativeSlack
    }
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
    domain
        .scaled(weight.get())
        .lower_bound_at_trail_position(assignments, trail_position)
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
