use itertools::Itertools;
use pumpkin_checking::AtomicConstraint;
use pumpkin_checking::InferenceChecker;
use pumpkin_checking::VariableState;

use crate::basic_types::PredicateId;
use crate::declare_inference_label;
use crate::predicates::Predicate;
use crate::predicates::PropositionalConjunction;
use crate::proof::ConstraintTag;
use crate::proof::InferenceCode;
use crate::propagation::ExplanationContext;
use crate::propagation::InferenceCheckers;
use crate::propagation::PropagationContext;
use crate::propagation::Propagator;
use crate::propagation::PropagatorConstructor;
use crate::propagation::PropagatorConstructorContext;
use crate::propagation::ReadDomains;
use crate::results::PropagationStatusCP;
use crate::state::PropagatorConflict;

/// The [`PropagatorConstructor`] for the [`SingleNogoodPropagatorPropagator`].
#[derive(Clone, Debug)]
pub struct SingleNogoodPropagatorConstructor {
    pub conjunction: PropositionalConjunction,
    pub constraint_tag: ConstraintTag,
}

impl PropagatorConstructor for SingleNogoodPropagatorConstructor {
    type PropagatorImpl = SingleNogoodPropagatorPropagator;

    fn add_inference_checkers(&self, mut checkers: InferenceCheckers<'_>) {
        checkers.add_inference_checker(
            InferenceCode::new(self.constraint_tag, Nogood),
            Box::new(NogoodChecker {
                conjunction: self.conjunction.iter().copied().collect(),
            }),
        );
    }

    fn create(self, mut context: PropagatorConstructorContext) -> Self::PropagatorImpl {
        let SingleNogoodPropagatorConstructor {
            conjunction,
            constraint_tag,
        } = self;

        assert!(!conjunction.is_empty());

        let watched_predicates = if conjunction.is_empty() {
            let true_predicate = Predicate::trivially_true();
            let true_predicate_id = context.register_predicate(true_predicate);
            [true_predicate_id; NUM_WATCHED_PREDICATES]
        } else {
            let last_idx = conjunction.len() - 1;
            [
                context.register_predicate(conjunction[0]),
                context.register_predicate(conjunction[1.min(last_idx)]),
            ]
        };

        SingleNogoodPropagatorPropagator {
            conjunction: conjunction.to_vec().into(),
            watched_predicates,
            inference_code: InferenceCode::new(constraint_tag, Nogood),
        }
    }
}

declare_inference_label!(Nogood);

const NUM_WATCHED_PREDICATES: usize = 2;

/// A [`Propagator`] for the individual nogoods.
#[derive(Clone, Debug)]
pub struct SingleNogoodPropagatorPropagator {
    conjunction: Box<[Predicate]>,
    /// The predicate ID at index i corresponds to the predicate at index i in
    /// `conjunction`.
    watched_predicates: [PredicateId; NUM_WATCHED_PREDICATES],

    inference_code: InferenceCode,
}

impl SingleNogoodPropagatorPropagator {
    /// Get the watcher index for the predicate that is unassigned.
    ///
    /// Assumes 0 or 1 watched predicates are/is unassigned.
    fn unassigned_watcher_index(&self, mut context: PropagationContext<'_>) -> Option<usize> {
        self.watched_predicates
            .iter()
            .position(|&pid| !context.is_predicate_id_satisfied(pid))
    }

    /// Updates the watched predicate at `watcher_index`.
    ///
    /// Returns true if a new unassigned predicate is now watched, or false if all other predicates
    /// are already true. If false is returned, the state of `self` is unaltered.
    fn find_new_watcher(
        &mut self,
        mut context: PropagationContext<'_>,
        watcher_index: usize,
    ) -> bool {
        let old_watcher = self.watched_predicates[watcher_index];

        let next_predicate_to_watch = self
            .conjunction
            .iter()
            .skip(NUM_WATCHED_PREDICATES)
            .position(|&predicate| context.evaluate_predicate(predicate) != Some(true))
            .map(|index| index + NUM_WATCHED_PREDICATES);

        if let Some(predicate_index) = next_predicate_to_watch {
            // To update the watcher we find a new predicate that is not assigned to true and put
            // it in the spot of the predicate that became true.

            let next_predicate_to_watch = self.conjunction[predicate_index];

            context.unregister_predicate(old_watcher);
            let new_predicate_id = context.register_predicate(next_predicate_to_watch);

            self.conjunction.swap(watcher_index, predicate_index);
            self.watched_predicates[watcher_index] = new_predicate_id;

            true
        } else {
            false
        }
    }

    /// Update the watched predicates of the conjunction.
    ///
    /// Returns the number of satisfied watchers, having tried replacing them with unassigned
    /// predicates.
    fn update_watched_predicates(&mut self, mut context: PropagationContext<'_>) -> usize {
        let mut satisfied_watchers = 0;

        for watcher_index in 0..self.watched_predicates.len() {
            let watched_predicate = self.watched_predicates[watcher_index];

            if context.is_predicate_id_satisfied(watched_predicate) {
                satisfied_watchers +=
                    usize::from(!self.find_new_watcher(context.reborrow(), watcher_index));
            }
        }

        satisfied_watchers
    }
}

impl Propagator for SingleNogoodPropagatorPropagator {
    fn name(&self) -> &str {
        "SingleNogoodPropagator"
    }

    fn propagate(&mut self, mut context: PropagationContext) -> PropagationStatusCP {
        let num_satisfied_watchers = self.update_watched_predicates(context.reborrow());

        if self.conjunction.len() > 1 && num_satisfied_watchers < NUM_WATCHED_PREDICATES - 1 {
            return Ok(());
        }

        let unassigned_watcher_index = self.unassigned_watcher_index(context.reborrow());

        match unassigned_watcher_index {
            Some(index) => {
                // We have one unassigned predicate in the conjunction. We put it at
                // position 0 in the conjunction so that we can filter it out in the
                // lazy explanation.
                self.watched_predicates.swap(0, index);
                self.conjunction.swap(0, index);

                let predicate_to_propagate = !self.conjunction[0];

                context.post(predicate_to_propagate, 0_u64, &self.inference_code)?;
            }

            None => {
                // All watchers are true, so the constraint is conflicting.
                return Err(PropagatorConflict {
                    conjunction: self.conjunction.iter().copied().collect(),
                    inference_code: self.inference_code.clone(),
                }
                .into());
            }
        }

        Ok(())
    }

    fn lazy_explanation(&mut self, code: u64, _: ExplanationContext) -> &[Predicate] {
        assert_eq!(code, 0);
        &self.conjunction[1..]
    }

    fn propagate_from_scratch(&self, mut context: PropagationContext) -> PropagationStatusCP {
        if self
            .conjunction
            .iter()
            .any(|&predicate| context.evaluate_predicate(predicate) == Some(false))
        {
            // If the conjunction contains at least one false predicate, the propagator will not do
            // anything.
            return Ok(());
        }

        // Get the predicates that are not assigned to true.
        let unsatisfied_predicates = self
            .conjunction
            .iter()
            .filter(|&&predicate| context.evaluate_predicate(predicate) != Some(true))
            .copied()
            .collect::<Vec<_>>();

        if unsatisfied_predicates.len() > 1 {
            // If more than one predicate remains unassigned, we cannot do anything.
            return Ok(());
        }

        if unsatisfied_predicates.len() == 1 {
            let unassigned_predicate = unsatisfied_predicates[0];

            context.post(
                !unassigned_predicate,
                self.conjunction
                    .iter()
                    .copied()
                    .collect::<PropositionalConjunction>(),
                &self.inference_code,
            )?;
        } else {
            return Err(PropagatorConflict {
                conjunction: self.conjunction.iter().copied().collect(),
                inference_code: self.inference_code.clone(),
            }
            .into());
        }

        Ok(())
    }
}

#[derive(Debug, Clone)]
struct NogoodChecker<Atomic> {
    pub conjunction: Box<[Atomic]>,
}

impl<Atomic> InferenceChecker<Atomic> for NogoodChecker<Atomic>
where
    Atomic: AtomicConstraint + Clone,
{
    fn check(&self, state: VariableState<Atomic>, _: &[Atomic], _: Option<&Atomic>) -> bool {
        self.conjunction.iter().all(|atomic| state.is_true(atomic))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::conjunction;
    use crate::predicate;
    use crate::state::State;

    #[test]
    fn conflict_detected() {
        let mut state = State::default();

        let x = state.new_interval_variable(2, 10, Some("x".into()));
        let y = state.new_interval_variable(2, 10, Some("y".into()));

        let conjunction = conjunction!([x >= 2] & [y >= 2]);
        let constraint_tag = state.new_constraint_tag();

        let _ = state.add_propagator(SingleNogoodPropagatorConstructor {
            conjunction,
            constraint_tag,
        });

        assert!(state.propagate_to_fixed_point().is_err());
    }

    #[test]
    fn incremental_conjunction_evaluation() {
        let mut state = State::default();

        let x = state.new_interval_variable(0, 10, Some("x".into()));
        let y = state.new_interval_variable(0, 10, Some("y".into()));
        let z = state.new_interval_variable(0, 5, Some("z".into()));

        let conjunction = conjunction!([x >= 2] & [y >= 2] & [z <= 3]);
        let constraint_tag = state.new_constraint_tag();

        let _ = state.add_propagator(SingleNogoodPropagatorConstructor {
            conjunction,
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
    fn backtracking_does_not_break_the_propagator() {
        let mut state = State::default();

        let x = state.new_interval_variable(0, 10, Some("x".into()));
        let y = state.new_interval_variable(0, 10, Some("y".into()));

        let conjunction = conjunction!([x >= 2] & [y >= 2]);
        let constraint_tag = state.new_constraint_tag();

        let _ = state.add_propagator(SingleNogoodPropagatorConstructor {
            conjunction,
            constraint_tag,
        });

        assert!(state.propagate_to_fixed_point().is_ok());

        state.new_checkpoint();

        assert!(state.post(predicate![x >= 2]).expect("not empty domain"));
        assert!(state.propagate_to_fixed_point().is_ok());
        let _ = state
            .post(predicate![y >= 2])
            .expect_err("empty domain on y");

        let _ = state.restore_to(0);

        assert!(state.post(predicate![y >= 2]).expect("not empty domain"));
        assert!(state.propagate_to_fixed_point().is_ok());

        assert_eq!(1, state.upper_bound(x));
    }
}
