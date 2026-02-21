use pumpkin_core::declare_inference_label;
use pumpkin_core::predicates::PropositionalConjunction;
use pumpkin_core::proof::ConstraintTag;
use pumpkin_core::proof::InferenceCode;
use pumpkin_core::propagation::PredicateId;
use pumpkin_core::propagation::PropagationContext;
use pumpkin_core::propagation::Propagator;
use pumpkin_core::propagation::PropagatorConstructor;
use pumpkin_core::propagation::PropagatorConstructorContext;
use pumpkin_core::propagation::ReadDomains;
use pumpkin_core::results::PropagationStatusCP;
use pumpkin_core::state::Conflict;
use pumpkin_core::state::PropagatorConflict;

#[derive(Clone, Debug)]
pub(crate) struct DeductionPropagatorArgs {
    pub(crate) nogood: PropositionalConjunction,
    pub(crate) constraint_tag: ConstraintTag,
}

impl PropagatorConstructor for DeductionPropagatorArgs {
    type PropagatorImpl = DeductionPropagator;

    fn create(self, mut context: PropagatorConstructorContext) -> Self::PropagatorImpl {
        declare_inference_label!(Nogood);

        let DeductionPropagatorArgs {
            nogood,
            constraint_tag,
        } = self;

        let ids = nogood
            .iter()
            .map(|&predicate| context.register_predicate(predicate))
            .collect();

        DeductionPropagator {
            nogood,
            ids,
            inference_code: InferenceCode::new(constraint_tag, Nogood),
            active: true,
        }
    }
}

#[derive(Clone, Debug)]
pub(crate) struct DeductionPropagator {
    nogood: PropositionalConjunction,
    ids: Vec<PredicateId>,
    active: bool,
    inference_code: InferenceCode,
}

impl DeductionPropagator {
    /// Prevent this propagator from doing anything in the future.
    ///
    /// Cannot be undone. This is the same as removing the propagator, but that is not
    /// supported at the moment.
    pub(crate) fn deactivate(&mut self) {
        self.active = false;
    }
}

impl Propagator for DeductionPropagator {
    fn name(&self) -> &str {
        "ProcessorNogoodPropagator"
    }

    fn propagate_from_scratch(&self, mut context: PropagationContext) -> PropagationStatusCP {
        if !self.active {
            for &predicate_id in self.ids.iter() {
                context.unregister_predicate(predicate_id);
            }
            return Ok(());
        }

        let num_assigned_predicates = self
            .nogood
            .iter()
            .filter(|&&predicate| context.evaluate_predicate(predicate) == Some(true))
            .count();

        let num_unassigned_predicates = self.nogood.len() - num_assigned_predicates;

        if num_unassigned_predicates == 0 {
            return Err(Conflict::Propagator(PropagatorConflict {
                conjunction: self.nogood.clone(),
                inference_code: self.inference_code.clone(),
            }));
        } else if num_unassigned_predicates == 1 {
            let unassigned_predicate = self
                .nogood
                .iter()
                .copied()
                .find(|&predicate| context.evaluate_predicate(predicate) != Some(true))
                .expect("exactly one predicate is not true");

            if context.evaluate_predicate(unassigned_predicate).is_none() {
                let explanation = self
                    .nogood
                    .iter()
                    .copied()
                    .filter(|&predicate| predicate != unassigned_predicate)
                    .collect::<PropositionalConjunction>();

                context.post(!unassigned_predicate, explanation, &self.inference_code)?;
            }
        }

        Ok(())
    }
}
