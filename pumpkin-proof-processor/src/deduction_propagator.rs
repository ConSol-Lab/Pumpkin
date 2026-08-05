use std::rc::Rc;

use pumpkin_core::declare_inference_label;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::predicates::PropositionalConjunction;
use pumpkin_core::proof::ConstraintTag;
use pumpkin_core::proof::InferenceCode;
use pumpkin_core::propagation::EventsToRegister;
use pumpkin_core::propagation::PredicateId;
use pumpkin_core::propagation::Priority;
use pumpkin_core::propagation::PropagationContext;
use pumpkin_core::propagation::Propagator;
use pumpkin_core::propagation::PropagatorConstructor;
use pumpkin_core::propagation::PropagatorConstructorContext;
use pumpkin_core::propagation::PropagatorSpec;
use pumpkin_core::propagation::ReadDomains;
use pumpkin_core::propagation::RuntimeCheckers;
use pumpkin_core::propagators::nogoods::NogoodChecker;
use pumpkin_core::state::Conflict;
use pumpkin_core::state::PropagationStatusCP;
use pumpkin_core::state::PropagatorConflict;

/// The [`PropagatorConstructor`] for the [`DeductionPropagator`].
#[derive(Clone, Debug)]
pub(crate) struct DeductionPropagatorConstructor {
    /// The nogood to propagate.
    pub(crate) nogood: Rc<[Predicate]>,
    /// The constraint tag of the nogood.
    pub(crate) constraint_tag: ConstraintTag,
    /// The priority of the propagator.
    pub(crate) priority: Priority,
    /// Whether this propagator will perform conflict detection XOR unit propagation
    pub(crate) propagation_mode: DeductionPropagationMode,
}

/// Used to indicate the propagation mode that is used
/// by the `DeductionPropagator`.
#[derive(Clone, Debug)]
pub(crate) enum DeductionPropagationMode {
    OnlyConflictDetection,
    OnlyUnitPropagation,
}

/// Conflict detection of unmarked deductions is given a higher priority
/// than unit propagation to greedily minimise the number unmarked deductions
/// that get marked.
pub(crate) const UNMARKED_CONFLICT_PRIORITY: Priority = Priority::UltraLow;

/// Unit propagation of unmarked deductions is given a lower priority
/// than conflict detection to greedily minimise the number of unmarked
/// deductions that get marked.
pub(crate) const UNMARKED_UNIT_PROPAGATION_PRIORITY: Priority = Priority::Lowest;

impl PropagatorConstructor for DeductionPropagatorConstructor {
    type PropagatorImpl = DeductionPropagator;

    fn create(
        self,
        mut context: PropagatorConstructorContext,
    ) -> PropagatorSpec<Self::PropagatorImpl> {
        declare_inference_label!(Nogood);

        let DeductionPropagatorConstructor {
            nogood,
            constraint_tag,
            priority,
            propagation_mode,
        } = self;
        let ids = nogood
            .iter()
            .map(|&predicate| context.register_predicate(predicate))
            .collect();

        let mut checkers = RuntimeCheckers::builder();
        let inference_code = checkers.add_inference_checker(
            constraint_tag,
            Nogood,
            NogoodChecker {
                nogood: nogood.iter().copied().collect(),
            },
        );

        let propagator = DeductionPropagator {
            nogood,
            ids,
            inference_code,
            active: true,
            propagation_priority: priority,
            propagation_mode,
        };

        PropagatorSpec {
            registration: EventsToRegister::empty(),
            checkers: checkers.build(),
            propagator,
        }
    }
}

/// A nogood propagator used to propagate deductions in the proof processor.
///
/// The main feature of this propagator is that it can be deactivated using
/// [`DeductionPropagator::deactivate`]. The proof processor uses this during backward trimming to
/// effectively remove constraints once it determined whether the constraint needs to be kept in
/// the processed proof.
#[derive(Clone, Debug)]
pub(crate) struct DeductionPropagator {
    /// The nogood to propagate.
    nogood: Rc<[Predicate]>,
    /// The IDs for the predicates in the nogood.
    ///
    /// The order in this vector is unspecified. In particular, it is not true that the ID at index
    /// i corresponds to the predicate at index i. This is fine since the IDs are only used to
    /// unwatch the predicates when the propagator is deactivated.
    ids: Vec<PredicateId>,
    /// If `true`, the propagator should propagate when enqueued. Otherwise, the propagator will do
    /// nothing if invoked.
    active: bool,
    /// The inference code for this propagator.
    inference_code: InferenceCode,
    /// The priority of this propagator.
    propagation_priority: Priority,
    /// Whether this propagator is part of the 'conflict detection' stage or the 'unit propagation'
    /// stage.
    propagation_mode: DeductionPropagationMode,
}

impl DeductionPropagator {
    /// Prevent this propagator from doing anything in the future.
    ///
    /// Cannot be undone. This is the same as removing the propagator, but that is not
    /// supported at the moment.
    pub(crate) fn deactivate(&mut self) {
        self.active = false;
    }

    pub(crate) fn set_priority(&mut self, new_priority: Priority) {
        self.propagation_priority = new_priority;
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

        // It should not be possible for an unmarked deduction to unit propagate
        // and cause a conflict, as a failure should have been declared by
        // the `DeductionPropagator` that does conflict detection,
        // which has a higher priority than its unit propagating counterpart.
        assert!(
            !(self.propagation_priority > Priority::VeryLow
                && matches!(
                    self.propagation_mode,
                    DeductionPropagationMode::OnlyUnitPropagation
                )
                && num_unassigned_predicates == 0)
        );

        if matches!(
            self.propagation_mode,
            DeductionPropagationMode::OnlyConflictDetection
        ) && num_unassigned_predicates == 0
        {
            return Err(Conflict::Propagator(PropagatorConflict {
                conjunction: self.nogood.iter().copied().collect(),
                inference_code: self.inference_code.clone(),
            }));
        } else if matches!(
            self.propagation_mode,
            DeductionPropagationMode::OnlyUnitPropagation
        ) && num_unassigned_predicates == 1
        {
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

                // This will never fail, as the predicate is known to be unassigned. So
                // this propagator only returns explicit conflicts and never empty
                // domain conflicts.
                context.post(!unassigned_predicate, (explanation, &self.inference_code))?;
            }
        }

        Ok(())
    }

    fn priority(&self) -> Priority {
        self.propagation_priority
    }
}
