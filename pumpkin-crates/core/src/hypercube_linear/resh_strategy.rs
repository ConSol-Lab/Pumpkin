use dyn_clone::DynClone;
use itertools::Itertools;
use log::trace;

use crate::create_statistics_struct;
use crate::hypercube_linear::LinearInequality;
use crate::hypercube_linear::conflict_state::ConflictState;
use crate::hypercube_linear::explanation::HypercubeLinearExplanation;
use crate::hypercube_linear::trail_view::TrailView;
use crate::hypercube_linear::trail_view::affine_lower_bound_at;
use crate::predicate;
use crate::predicates::Predicate;
use crate::statistics::Statistic;
use crate::statistics::StatisticLogger;

pub(crate) trait ResHStrategy: DynClone + std::fmt::Debug {
    fn apply(
        &mut self,
        state: &mut ConflictState,
        trail: &mut dyn TrailView,
        trail_position: usize,
        pivot: Predicate,
        explanation: HypercubeLinearExplanation,
    );

    fn log_statistics(&self, _logger: StatisticLogger) {}
}

dyn_clone::clone_trait_object!(ResHStrategy);

// ======== StandardResH ========

create_statistics_struct!(StandardResHStatistics {
    num_propositional_resolutions_use_explanation_linear: usize,
});

#[derive(Clone, Debug, Default)]
pub(crate) struct StandardResH {
    statistics: StandardResHStatistics,
}

impl ResHStrategy for StandardResH {
    fn apply(
        &mut self,
        state: &mut ConflictState,
        trail: &mut dyn TrailView,
        trail_position: usize,
        pivot: Predicate,
        mut explanation: HypercubeLinearExplanation,
    ) {
        let linear_propagated_pivot_to_false = explanation.iter_predicates().any(|p| p == !pivot);

        let linear_slack_is_negative = if let Some(linear) = explanation.linear() {
            compute_linear_slack_at_trail_position(trail, linear, trail_position - 1) < 0
        } else {
            true
        };

        let can_substitute_with_explanation_linear =
            linear_propagated_pivot_to_false && linear_slack_is_negative;

        if state.conflicting_linear.is_trivially_false() && can_substitute_with_explanation_linear {
            // If the conflicting linear is a clause, then we do not need to clausify
            // the explanation. Instead, the linear of the conflicting constraint
            // becomes the linear of the explanation and the hypercube of the conflict
            // is extended with the hypercube of the conflict.

            trace!(
                "since the linear in the conflict is trivially false, use linear from explanation"
            );

            for predicate in explanation.iter_predicates() {
                add_true_part_of_predicate(state, trail, trail_position, predicate);
            }

            let linear = explanation.take_linear();
            state.explain_linear(trail, &linear, trail_position - 1);
            state.conflicting_linear = linear;

            self.statistics
                .num_propositional_resolutions_use_explanation_linear += 1;
        } else {
            let clausal_explanation = explanation.into_clause(trail, pivot, trail_position);

            trace!(
                "clausal explanation: {}",
                clausal_explanation.iter().format(" & ")
            );
            for predicate in clausal_explanation {
                add_true_part_of_predicate(state, trail, trail_position, predicate);
            }
        }
    }

    fn log_statistics(&self, logger: StatisticLogger) {
        self.statistics.log(logger);
    }
}

/// Adds `predicate` from the explanation to the hypercube of the conflict if it is true at
/// `trail_position`.
///
/// A predicate that is false contains the negation of the pivot, which propositional resolution
/// removes. An equality `[x == v]` can be false while one of its bounds is true, e.g. when
/// weakening on the negation `[x >= v]` of the pivot `[x <= v - 1]` merged `[x >= v]` with `[x <=
/// v]`. That true bound restricts the values of `x` for which the explanation holds, so it is
/// added. Without it, the resolvent would not be implied.
fn add_true_part_of_predicate(
    state: &mut ConflictState,
    trail: &dyn TrailView,
    trail_position: usize,
    predicate: Predicate,
) {
    let truth_value = trail
        .truth_value_at(predicate, trail_position)
        .expect("all predicates in explanation hypercube are assigned");

    if truth_value {
        state.add_hypercube_predicate(trail, predicate);
    } else if predicate.is_equality_predicate() {
        let domain = predicate.get_domain();
        let value = predicate.get_right_hand_side();

        for bound in [predicate![domain >= value], predicate![domain <= value]] {
            if trail.truth_value_at(bound, trail_position) == Some(true) {
                state.add_hypercube_predicate(trail, bound);
            }
        }
    }
}

fn compute_linear_slack_at_trail_position(
    trail: &dyn TrailView,
    linear: &LinearInequality,
    trail_position: usize,
) -> i64 {
    let lower_bound_terms = linear
        .terms()
        .map(|term| affine_lower_bound_at(trail, term, trail_position))
        .sum::<i64>();

    i64::from(linear.bound()) - lower_bound_terms
}
