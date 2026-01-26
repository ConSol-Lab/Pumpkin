mod hypercube;
mod linear;

pub use hypercube::*;
pub use linear::*;

use crate::basic_types::PredicateId;
use crate::declare_inference_label;
use crate::predicate;
use crate::predicates::Predicate;
use crate::proof::ConstraintTag;
use crate::proof::InferenceCode;
use crate::propagation::EnqueueDecision;
use crate::propagation::NotificationContext;
use crate::propagation::PropagationContext;
use crate::propagation::Propagator;
use crate::propagation::PropagatorConstructor;
use crate::propagation::PropagatorConstructorContext;
use crate::propagation::ReadDomains;
use crate::pumpkin_assert_eq_advanced;
use crate::results::PropagationStatusCP;
use crate::state::Conflict;
use crate::state::PropagatorConflict;

/// The [`PropagatorConstructor`] for the [`HypercubeLinearPropagator`].
#[derive(Clone, Debug)]
pub struct HypercubeLinearConstructor {
    pub hypercube: Hypercube,
    pub linear: LinearInequality,
    pub constraint_tag: ConstraintTag,
}

impl PropagatorConstructor for HypercubeLinearConstructor {
    type PropagatorImpl = HypercubeLinearPropagator;

    fn create(self, mut context: PropagatorConstructorContext) -> Self::PropagatorImpl {
        let HypercubeLinearConstructor {
            hypercube,
            linear,
            constraint_tag,
        } = self;

        let hypercube_predicates = hypercube.iter_predicates().collect::<Box<[_]>>();

        let watched_predicates = if hypercube_predicates.is_empty() {
            let true_predicate = Predicate::trivially_true();
            let true_predicate_id = context.register_predicate(true_predicate);
            [true_predicate_id; 3]
        } else {
            let last_idx = hypercube_predicates.len() - 1;
            [
                context.register_predicate(hypercube_predicates[0.min(last_idx)]),
                context.register_predicate(hypercube_predicates[1.min(last_idx)]),
                context.register_predicate(hypercube_predicates[2.min(last_idx)]),
            ]
        };

        HypercubeLinearPropagator {
            hypercube,
            linear,

            hypercube_predicates,
            watched_predicates,
            constraint_tag,
        }
    }
}

declare_inference_label!(HypercubeLinear);

const NUM_WATCHED_PREDICATES: usize = 3;

/// A [`Propagator`] for the hypercube linear constraint.
#[derive(Clone, Debug)]
pub struct HypercubeLinearPropagator {
    hypercube: Hypercube,
    linear: LinearInequality,

    hypercube_predicates: Box<[Predicate]>,
    /// The predicate ID at index i corresponds to the predicate at index i in
    /// `hypercube_predicates`.
    watched_predicates: [PredicateId; NUM_WATCHED_PREDICATES],

    constraint_tag: ConstraintTag,
}

impl HypercubeLinearPropagator {
    /// Get the index from [`Self::watched_predicates`] that corresponds to `predicate_id`.
    ///
    /// If no such index exists, a panic occurs.
    fn watcher_index_for(&self, predicate_id: PredicateId) -> usize {
        self.watched_predicates
            .iter()
            .position(|&pid| pid == predicate_id)
            .expect("can only get the watcher index for watched predicates")
    }
}

impl Propagator for HypercubeLinearPropagator {
    fn name(&self) -> &str {
        "HypercubeLinear"
    }

    fn notify_predicate_id_satisfied(
        &mut self,
        mut context: NotificationContext,
        predicate_id: PredicateId,
    ) -> EnqueueDecision {
        let watcher_index = self.watcher_index_for(predicate_id);

        pumpkin_assert_eq_advanced!(
            Some(true),
            context.evaluate_predicate(self.hypercube_predicates[watcher_index])
        );

        let next_predicate_to_watch = self.hypercube_predicates[NUM_WATCHED_PREDICATES..]
            .iter()
            .position(|&predicate| context.evaluate_predicate(predicate) != Some(true))
            .map(|index| index + NUM_WATCHED_PREDICATES);

        if let Some(predicate_index) = next_predicate_to_watch {
            // To update the watcher we find a new predicate that is not assigned to true and put
            // it in the spot of the predicate that became true.

            let next_predicate_to_watch = self.hypercube_predicates[predicate_index];

            context.unwatch_predicate(predicate_id);
            let new_predicate_id = context.watch_predicate(next_predicate_to_watch);

            self.hypercube_predicates
                .swap(watcher_index, predicate_index);
            self.watched_predicates[watcher_index] = new_predicate_id;
        }

        let satisfied_watchers = self
            .watched_predicates
            .iter()
            .filter(|&&predicate_id| context.is_predicate_id_satisfied(predicate_id))
            .count();

        if satisfied_watchers >= NUM_WATCHED_PREDICATES - 1 {
            EnqueueDecision::Enqueue
        } else {
            EnqueueDecision::Skip
        }
    }

    fn propagate(&mut self, context: PropagationContext) -> PropagationStatusCP {
        let is_hypercube_satisfied = self
            .hypercube_predicates
            .iter()
            .all(|&predicate| context.evaluate_predicate(predicate) == Some(true));

        if !is_hypercube_satisfied {
            return Ok(());
        }

        let lower_bound_terms = self
            .linear
            .terms()
            .map(|term| i64::from(context.lower_bound(&term)))
            .sum::<i64>();

        if lower_bound_terms > i64::from(self.linear.bound()) {
            let conjunction = self
                .linear
                .terms()
                .map(|term| predicate![term >= context.lower_bound(&term)])
                .chain(self.hypercube.iter_predicates())
                .collect();

            Err(Conflict::Propagator(PropagatorConflict {
                conjunction,
                inference_code: InferenceCode::new(self.constraint_tag, HypercubeLinear),
            }))
        } else {
            Ok(())
        }
    }

    fn propagate_from_scratch(&self, context: PropagationContext) -> PropagationStatusCP {
        let is_hypercube_satisfied = self
            .hypercube_predicates
            .iter()
            .all(|&predicate| context.evaluate_predicate(predicate) == Some(true));

        if !is_hypercube_satisfied {
            return Ok(());
        }

        let lower_bound_terms = self
            .linear
            .terms()
            .map(|term| i64::from(context.lower_bound(&term)))
            .sum::<i64>();

        if lower_bound_terms > i64::from(self.linear.bound()) {
            let conjunction = self
                .linear
                .terms()
                .map(|term| predicate![term >= context.lower_bound(&term)])
                .chain(self.hypercube.iter_predicates())
                .collect();

            Err(Conflict::Propagator(PropagatorConflict {
                conjunction,
                inference_code: InferenceCode::new(self.constraint_tag, HypercubeLinear),
            }))
        } else {
            Ok(())
        }
    }
}

#[cfg(test)]
mod tests {
    use std::num::NonZero;

    use super::*;
    use crate::predicate;
    use crate::state::State;

    #[test]
    fn conflict_detected() {
        let mut state = State::default();

        let x = state.new_interval_variable(2, 10, Some("x".into()));
        let y = state.new_interval_variable(2, 10, Some("y".into()));
        let z = state.new_interval_variable(2, 5, Some("z".into()));

        let hypercube =
            Hypercube::new([predicate![x >= 2], predicate![y >= 2]]).expect("not inconsistent");
        // x + y + z <= 5.
        let linear = LinearInequality::new(
            [
                (NonZero::new(1).unwrap(), x),
                (NonZero::new(1).unwrap(), y),
                (NonZero::new(1).unwrap(), z),
            ],
            5,
        )
        .expect("not trivially true");

        let constraint_tag = state.new_constraint_tag();
        let _ = state.add_propagator(HypercubeLinearConstructor {
            hypercube,
            linear,
            constraint_tag,
        });

        assert!(state.propagate_to_fixed_point().is_err());
    }

    #[test]
    fn incremental_hypercube_evaluation() {
        let mut state = State::default();

        let x = state.new_interval_variable(0, 10, Some("x".into()));
        let y = state.new_interval_variable(0, 10, Some("y".into()));
        let z = state.new_interval_variable(0, 5, Some("z".into()));

        let hypercube =
            Hypercube::new([predicate![x >= 2], predicate![y >= 2], predicate![z <= 3]])
                .expect("not inconsistent");

        let linear = LinearInequality::trivially_false();

        let constraint_tag = state.new_constraint_tag();
        let _ = state.add_propagator(HypercubeLinearConstructor {
            hypercube,
            linear,
            constraint_tag,
        });

        assert!(state.propagate_to_fixed_point().is_ok());

        let _ = state.post(predicate![x >= 2]).expect("domain not empty");
        assert!(state.propagate_to_fixed_point().is_ok());
        let _ = state.post(predicate![y >= 2]).expect("domain not empty");
        let _ = state.post(predicate![z <= 3]).expect("domain not empty");
        assert!(state.propagate_to_fixed_point().is_err());
    }

    #[test]
    fn empty_hypercube_simplifies_to_linear_conflict() {
        let mut state = State::default();

        let x = state.new_interval_variable(2, 10, Some("x".into()));
        let y = state.new_interval_variable(2, 10, Some("y".into()));
        let z = state.new_interval_variable(2, 5, Some("z".into()));

        let hypercube = Hypercube::new([]).expect("not inconsistent");
        // x + y + z <= 5.
        let linear = LinearInequality::new(
            [
                (NonZero::new(1).unwrap(), x),
                (NonZero::new(1).unwrap(), y),
                (NonZero::new(1).unwrap(), z),
            ],
            5,
        )
        .expect("not trivially true");

        let constraint_tag = state.new_constraint_tag();
        let _ = state.add_propagator(HypercubeLinearConstructor {
            hypercube,
            linear,
            constraint_tag,
        });

        assert!(state.propagate_to_fixed_point().is_err());
    }
}
