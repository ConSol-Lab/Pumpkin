use std::num::NonZero;

use itertools::Itertools;
use log::debug;
use log::trace;

use crate::basic_types::StoredConflictInfo;
use crate::basic_types::moving_averages::CumulativeMovingAverage;
use crate::basic_types::moving_averages::MovingAverage;
use crate::create_statistics_struct;
use crate::engine::Assignments;
use crate::engine::conflict_analysis::ConflictAnalysisContext;
use crate::engine::conflict_analysis::ConflictResolver;
use crate::engine::propagation::PropagationContextMut;
use crate::engine::propagation::Propagator;
use crate::engine::propagation::constructor::PropagatorConstructor;
use crate::engine::propagation::constructor::PropagatorConstructorContext;
use crate::engine::propagation::store::PropagatorStore;
use crate::engine::reason::Reason;
use crate::engine::reason::ReasonRef;
use crate::engine::reason::ReasonStore;
use crate::engine::reason::StoredReason;
use crate::predicate;
use crate::predicates::Predicate;
use crate::predicates::PredicateType;
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

        trace!("constraint ID = {:?}", propagator.constraint_tag);

        (propagator.hypercube_linear.clone(), reason_set)
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

        // for p in learned_constraint.iter_hypercube() {
        //     let domain = p.get_domain();
        //     println!(
        //         "{} in [{}, {}]",
        //         domain,
        //         domain.lower_bound(context.assignments),
        //         domain.lower_bound(context.assignments)
        //     );
        // }
        // for (_, domain) in learned_constraint.iter_linear_terms() {
        //     println!(
        //         "{} in [{}, {}]",
        //         domain,
        //         domain.lower_bound(context.assignments),
        //         domain.upper_bound(context.assignments)
        //     );
        // }

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
        context
            .propagator_queue
            .enqueue_propagator(new_propagator_id.propagator_id(), 0);

        context.solver_state.declare_solving();
    }

    fn learn_hypercube_linear(
        &mut self,
        context: HlResolverContext,
        mut conflicting_hypercube_linear: HypercubeLinear,
        mut conflicting_reason_set: Vec<Predicate>,
    ) -> (HypercubeLinear, Vec<Predicate>) {
        debug!(
            "Resolving conflict (dl = {}) {conflicting_hypercube_linear}",
            context.assignments.get_decision_level()
        );

        let conflict_dl = context.assignments.get_decision_level();
        let mut trail_index = context.assignments.num_trail_entries();

        // Iterate the trail backwards until a constraint is obtained that propagates at a
        // previous decision level.
        loop {
            trail_index -= 1;

            // println!("{conflicting_hypercube_linear:?}");
            // dbg!(&conflicting_reason_set);

            // for p in conflicting_reason_set.iter() {
            //     let domain = p.get_domain();
            //     println!(
            //         "{} in [{}, {}]",
            //         domain,
            //         domain.lower_bound_at_trail_position(context.assignments, trail_index),
            //         domain.upper_bound_at_trail_position(context.assignments, trail_index)
            //     );
            // }

            assert_eq!(
                is_conflicting(
                    &conflicting_hypercube_linear,
                    trail_index,
                    context.assignments,
                ),
                ConflictingStatus::Conflicting
            );

            assert!(conflicting_reason_set.iter().all(|predicate| {
                context
                    .assignments
                    .evaluate_predicate_at_trail_position(*predicate, trail_index)
                    == Some(true)
            }));

            let reasons_on_current_dl = conflicting_reason_set
                .iter()
                .filter(|predicate| {
                    context
                        .assignments
                        .get_decision_level_for_predicate(predicate)
                        .unwrap()
                        == conflict_dl
                })
                .count();

            trace!(
                "reasons on conflict dl = {reasons_on_current_dl} (out of {})",
                conflicting_reason_set.len()
            );

            if reasons_on_current_dl <= 1 {
                return (conflicting_hypercube_linear, conflicting_reason_set);
            }

            let top_of_trail = context.assignments.get_trail_entry(trail_index);
            assert!(
                top_of_trail.predicate.is_lower_bound_predicate()
                    || top_of_trail.predicate.is_upper_bound_predicate()
            );

            // Remove the top of trail from the reason set if it is in the reason set.
            if !conflicting_reason_set.contains(&top_of_trail.predicate) {
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

            // let reason_set_for_explanation =
            //     explain_propagation(&reason, trail_index, context.assignments, pivot_predicate);
            // conflicting_reason_set.extend(reason_set_for_explanation);

            let mut elimination_happened = false;

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
                None => {
                    trace!("no fourier resolution");
                }
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
            conflicting_hypercube_linear,
            reason_set,
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

        self.restore_solver(context, backjump_level, learned_hypercube_linear);
        true
    }
}

fn explain_propagation(
    hypercube_linear: &HypercubeLinear,
    trail_position: usize,
    assignments: &Assignments,
    predicate_to_explain: Predicate,
) -> Vec<Predicate> {
    // The given predicated can be propagated in two ways.
    //
    // Either its negation is in the hypercube, in which case the slack of the constraint must be
    // negative. Or it is propagated by the linear component, in which case all predicates in the
    // hypercube must be satisfied.

    let slack = compute_slack(hypercube_linear, trail_position, assignments);

    if slack < 0 {
        // The predicate is propagated by the hypercube. That means its negation is in the
        // hypercube and we filter it out of the explanation

        hypercube_linear
            .iter_hypercube()
            .filter(|predicate| *predicate != !predicate_to_explain)
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
            .collect()
    } else {
        // The predicate is propagated by the linear, which means we explain every term that is a
        // different domain than the one of the propagated predicate.
        hypercube_linear
            .iter_hypercube()
            .chain(
                hypercube_linear
                    .iter_linear_terms()
                    .filter_map(|(weight, domain)| {
                        if domain == predicate_to_explain.get_domain() {
                            None
                        } else {
                            let term = domain.scaled(weight.get());
                            let term_lower_bound =
                                term.lower_bound_at_trail_position(assignments, trail_position);
                            Some(predicate![term >= term_lower_bound])
                        }
                    }),
            )
            .collect()
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
) -> Vec<Predicate> {
    hypercube_linear
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
        .collect()
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
        .collect();

    let new_constraint = HypercubeLinear::new(
        hypercube,
        conflicting_hypercube_linear.iter_linear_terms().collect(),
        conflicting_hypercube_linear.linear_rhs(),
    );

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
    conflicting_hypercube_linear: &HypercubeLinear,
    reason: &HypercubeLinear,
    pivot_predicate: Predicate,
) -> Option<HypercubeLinear> {
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
            _ => return None,
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

    let new_constraint = HypercubeLinear::new(hypercube, linear_terms, linear_rhs);

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
) -> Option<(usize, Predicate)> {
    // Keeps track of what would propagate, and whether it would cause an empty domain. We only
    // want to determine we are done if we do _not_ cause a conflict at a decision level that is
    // lower.
    let mut to_propagate = None;
    let mut causes_conflict = false;

    let current_decision_level = assignments.get_decision_level();

    for decision_level in (0..=current_decision_level - 1).rev() {
        let trail_position = assignments.get_position_at_decision_level(decision_level);
        trace!(
            "testing propagation at dl={decision_level} (tp={trail_position}, curdl={current_decision_level})"
        );

        if let Some(predicate) =
            is_propagating(conflicting_hypercube_linear, trail_position, assignments)
        {
            let slack = compute_slack(conflicting_hypercube_linear, trail_position, assignments);
            causes_conflict = slack < 0;
            trace!("propagating at dl={decision_level} ({predicate}, slack={slack})");
            to_propagate = Some((decision_level, predicate));
        } else {
            trace!("not propagating at dl={}", decision_level);
            break;
        }
    }

    if causes_conflict { None } else { to_propagate }
}

/// Returns the predicate to propagate if the given hypercube linear is propagating at the given trail position.
fn is_propagating(
    hypercube_linear: &HypercubeLinear,
    trail_position: usize,
    assignments: &Assignments,
) -> Option<Predicate> {
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
    let can_propagate_linear = satisfied_bounds_in_hypercube > 0
        && satisfied_bounds_in_hypercube == hypercube_linear.iter_hypercube().len() - 1;
    if can_propagate_linear && slack < 0 {
        let predicate_to_propagate = hypercube_linear
            .iter_hypercube()
            .find(|predicate| {
                assignments
                    .evaluate_predicate_at_trail_position(*predicate, trail_position)
                    .is_none()
            })
            .expect("there should be exactly one unassigned predicate");

        trace!("{hypercube_linear} would propagate {predicate_to_propagate}");

        return Some(predicate_to_propagate);
    }

    // All bounds in the hypercube are satisfied, meaning the linear term _could_ propagate.
    if satisfied_bounds_in_hypercube == hypercube_linear.iter_hypercube().len() {
        // Determine whether an upper bound exceeds what would be propagated.
        return hypercube_linear
            .iter_linear_terms()
            .find_map(|(weight, domain)| {
                let term = domain.scaled(weight.get());

                let term_lower_bound =
                    term.lower_bound_at_trail_position(assignments, trail_position);
                let term_upper_bound =
                    term.upper_bound_at_trail_position(assignments, trail_position);
                let bound = slack + term_lower_bound;

                if term_upper_bound > bound {
                    let to_propagate = predicate![term <= bound];

                    trace!("{hypercube_linear} would propagate {to_propagate}",);
                    Some(to_propagate)
                } else {
                    None
                }
            });
    }

    None
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

    // #[test]
    // fn resolution_test() {
    //     env_logger::init();

    //     let mut constraint_tags = KeyGenerator::default();
    //     let mut inference_codes = KeyGenerator::default();

    //     let mut notification_engine = NotificationEngine::default();
    //     notification_engine.grow();
    //     notification_engine.grow();
    //     notification_engine.grow();

    //     let mut assignments = Assignments::default();
    //     let x = assignments.grow(2, 10);
    //     let y = assignments.grow(0, 10);
    //     let z = assignments.grow(0, 10);

    //     let mut reason_store = ReasonStore::default();
    //     let mut propagators = PropagatorStore::default();

    //     let inf_a = inference_codes.next_key();
    //     let c_a = propagators
    //         .new_propagator()
    //         .populate(HypercubeLinearPropagator::new(
    //             HypercubeLinear::new(
    //                 vec![predicate![x >= 1]],
    //                 vec![(NonZero::new(1).unwrap(), y), (NonZero::new(1).unwrap(), z)],
    //                 5,
    //             ),
    //             constraint_tags.next_key(),
    //             inf_a,
    //         ));

    //     let c_b = propagators
    //         .new_propagator()
    //         .populate(HypercubeLinearPropagator::new(
    //             HypercubeLinear::new(
    //                 vec![predicate![y >= 1]],
    //                 vec![(NonZero::new(-1).unwrap(), z)],
    //                 -2,
    //             ),
    //             constraint_tags.next_key(),
    //             inference_codes.next_key(),
    //         ));

    //     assignments.increase_decision_level();
    //     reason_store.increase_decision_level();
    //     let _ = assignments
    //         .post_predicate(predicate![x >= 1], None, &mut notification_engine)
    //         .unwrap();
    //     assignments.increase_decision_level();
    //     reason_store.increase_decision_level();
    //     let _ = assignments
    //         .post_predicate(predicate![y >= 4], None, &mut notification_engine)
    //         .unwrap();

    //     let reason_ref = reason_store.push(c_a.propagator_id(), StoredReason::DynamicLazy(0));
    //     let _ = assignments
    //         .post_predicate(
    //             predicate![z <= 1],
    //             Some((reason_ref, inf_a)),
    //             &mut notification_engine,
    //         )
    //         .unwrap();

    //     let mut resolver = HypercubeLinearResolver::default();
    //     let conflicting_constraint = propagators
    //         .get_propagator(c_b)
    //         .unwrap()
    //         .hypercube_linear
    //         .clone();

    //     let (backjump_level, learned_constraint) = resolver.learn_hypercube_linear(
    //         HlResolverContext {
    //             assignments: &assignments,
    //             reason_store: &mut reason_store,
    //             propagators: &propagators,
    //         },
    //         conflicting_constraint,
    //     );

    //     assert_eq!(backjump_level, 1);

    //     let expected_constraint =
    //         HypercubeLinear::new(vec![predicate![x >= 1], predicate![y >= 4]], vec![], -1);

    //     assert_eq!(learned_constraint, expected_constraint);
    // }
}
