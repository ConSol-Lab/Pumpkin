use std::num::NonZero;

use crate::basic_types::moving_averages::CumulativeMovingAverage;
use crate::basic_types::moving_averages::MovingAverage;
use crate::basic_types::StoredConflictInfo;
use crate::create_statistics_struct;
use crate::engine::conflict_analysis::ConflictAnalysisContext;
use crate::engine::conflict_analysis::ConflictResolver;
use crate::engine::propagation::constructor::PropagatorConstructor;
use crate::engine::propagation::constructor::PropagatorConstructorContext;
use crate::engine::reason::ReasonRef;
use crate::engine::Assignments;
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

    /// Get the hypercube linear reason for the propagation.
    fn get_reason(
        &self,
        context: &mut ConflictAnalysisContext<'_>,
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

    fn restore_solver(
        &mut self,
        mut context: ConflictAnalysisContext<'_>,
        decision_level: usize,
        learned_constraint: HypercubeLinear,
    ) {
        self.statistics
            .average_num_linear_terms_in_learned_hypercube_linear
            .add_term(learned_constraint.iter_linear_terms().len());
        self.statistics.num_learned_hypercube_linear += 1;

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

    /// Apply fourier elimination between the conflicting hypercube linear and the next reason.
    ///
    /// If there is no possible combination, then this returns the conflicting hypercube linear
    /// that was given as input.
    fn fourier_eliminate(
        &self,
        conflicting_hypercube_linear: HypercubeLinear,
        reason: HypercubeLinear,
        domain: DomainId,
    ) -> HypercubeLinear {
        let maybe_weight_in_conflicting = conflicting_hypercube_linear.variable_weight(domain);
        let maybe_weight_in_reason = reason.variable_weight(domain);

        let (weight_in_conflicting, weight_in_reason) =
            match (maybe_weight_in_conflicting, maybe_weight_in_reason) {
                (Some(a_weight), Some(b_weight)) if a_weight.get() * b_weight.get() < 0 => {
                    (a_weight, b_weight)
                }

                // Either the domain is not in one of the two constraints, or they don't have
                // opposing signs. In both cases, we cannot perform fourier
                // elimination on the specified domain.
                _ => return conflicting_hypercube_linear,
            };

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

        HypercubeLinear::new(hypercube, linear_terms, linear_rhs)
    }
}

impl ConflictResolver for HypercubeLinearResolver {
    fn log_statistics(&self, statistic_logger: crate::statistics::StatisticLogger) {
        self.statistics.log(statistic_logger);
    }

    fn resolve_conflict(&mut self, mut context: ConflictAnalysisContext) -> bool {
        let mut conflicting_hypercube_linear =
            self.compute_conflicting_hypercube_linear(&mut context);

        let mut trail_index = context.assignments.num_trail_entries() - 1;

        loop {
            assert!(is_conflicting(
                &conflicting_hypercube_linear,
                trail_index,
                context.assignments,
            ));

            let top_of_trail = context.assignments.get_trail_entry(trail_index);
            assert!(
                top_of_trail.predicate.is_lower_bound_predicate()
                    || top_of_trail.predicate.is_upper_bound_predicate()
            );

            let Some((reason, _)) = top_of_trail.reason else {
                let backjump_level = context.assignments.get_decision_level() - 1;
                self.restore_solver(context, backjump_level, conflicting_hypercube_linear);

                return true;
            };

            let reason = self.get_reason(&mut context, reason);
            assert!(reason
                .iter_hypercube()
                .all(|predicate| context.assignments.is_predicate_satisfied(predicate)), "since we do not watch the linear part, reasons must always have satisfied hypercubes");

            conflicting_hypercube_linear = self.fourier_eliminate(
                conflicting_hypercube_linear,
                reason,
                top_of_trail.predicate.get_domain(),
            );

            assert!(is_conflicting(
                &conflicting_hypercube_linear,
                trail_index,
                context.assignments
            ));

            // If the conflicting constraint would have propagated at the previous decision level,
            // then we want to stop and backtrack as far as possible.
            let backjump_to =
                determine_backjump(&conflicting_hypercube_linear, context.assignments);

            if backjump_to < context.assignments.get_decision_level() {
                self.restore_solver(context, backjump_to, conflicting_hypercube_linear);
                return true;
            }

            trail_index -= 1;
        }
    }
}

fn determine_backjump(
    conflicting_hypercube_linear: &HypercubeLinear,
    assignments: &mut Assignments,
) -> usize {
    for decision_level in (0..=assignments.get_decision_level() - 1).rev() {
        let trail_position = assignments.get_position_at_decision_level(decision_level);

        if is_propagating(conflicting_hypercube_linear, trail_position, assignments) {
            return decision_level + 1;
        }
    }

    unreachable!()
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
    let lhs = evaluate_linear(hypercube_linear, trail_position, assignments);

    hypercube_linear.iter_linear_terms().any(|term| {
        let term_lower_bound = term_lower_bound(term, trail_position, assignments);
        let term_upper_bound = term_upper_bound(term, trail_position, assignments);
        let bound = hypercube_linear.linear_rhs() - (lhs - term_lower_bound);

        term_upper_bound > bound
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

    // Evaluate the left-hand side.
    let lhs = evaluate_linear(hypercube_linear, trail_position, assignments);

    // The constraint is conflicting if lhs exceeds rhs.
    lhs > hypercube_linear.linear_rhs()
}

fn evaluate_linear(
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
