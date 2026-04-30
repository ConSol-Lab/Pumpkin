use dyn_clone::DynClone;
use itertools::Itertools;
use log::trace;

use crate::create_statistics_struct;
use crate::hypercube_linear::conflict_state::ConflictState;
use crate::hypercube_linear::explanation::HypercubeLinearExplanation;
use crate::hypercube_linear::trail_view::TrailView;
use crate::hypercube_linear::trail_view::affine_lower_bound_at;
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
                let truth_value = trail
                    .truth_value_at(predicate, trail_position)
                    .expect("all predicates in explanation hypercube are assigned");

                if !truth_value {
                    continue;
                }

                state.add_hypercube_predicate(trail, predicate);
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
                let truth_value = trail
                    .truth_value_at(predicate, trail_position)
                    .expect("all predicates in explanation hypercube are assigned");

                if !truth_value {
                    continue;
                }

                state.add_hypercube_predicate(trail, predicate);
            }
        }
    }

    fn log_statistics(&self, logger: StatisticLogger) {
        self.statistics.log(logger);
    }
}

fn compute_linear_slack_at_trail_position(
    trail: &dyn TrailView,
    linear: &crate::hypercube_linear::LinearInequality,
    trail_position: usize,
) -> i64 {
    let lower_bound_terms = linear
        .terms()
        .map(|term| i64::from(affine_lower_bound_at(trail, term, trail_position)))
        .sum::<i64>();

    i64::from(linear.bound()) - lower_bound_terms
}
