use std::rc::Rc;
use std::sync::OnceLock;
use std::sync::atomic::AtomicU64;
use std::sync::atomic::Ordering as AtomicOrdering;

use pumpkin_core::declare_inference_label;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::predicates::PropositionalConjunction;
use pumpkin_core::proof::ConstraintTag;
use pumpkin_core::proof::InferenceCode;
use pumpkin_core::propagation::EnqueueDecision;
use pumpkin_core::propagation::EventsToRegister;
use pumpkin_core::propagation::NotificationContext;
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

/// Both conflict detection and unit propagation of marked deductions have a
/// higher propagation priority than the propagators of unmarked deductions.
pub(crate) const MARKED_PROPAGATION_PRIORITY: Priority = Priority::VeryLow;

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

        let watched = [0, nogood.len().saturating_sub(1)];

        let propagator = DeductionPropagator {
            nogood,
            ids,
            inference_code,
            active: true,
            propagation_priority: priority,
            propagation_mode,
            watched,
        };

        PropagatorSpec {
            registration: EventsToRegister::empty(),
            checkers: checkers.build(),
            propagator,
        }
    }
}

/// Measurement switches, read once.
fn notify_skip_enabled() -> bool {
    static FLAG: OnceLock<bool> = OnceLock::new();
    *FLAG.get_or_init(|| std::env::var_os("PUMPKIN_PROCESSOR_NOTIFY_SKIP").is_some())
}

fn watches_disabled() -> bool {
    static FLAG: OnceLock<bool> = OnceLock::new();
    *FLAG.get_or_init(|| std::env::var_os("PUMPKIN_PROCESSOR_NO_WATCHES").is_some())
}

/// Instrumentation: how often the deduction propagators run and how often the watched pair lets
/// them return immediately.
pub(crate) static PROPAGATE_CALLS: AtomicU64 = AtomicU64::new(0);
pub(crate) static PROPAGATE_SHORTCUTS: AtomicU64 = AtomicU64::new(0);
pub(crate) static PROPAGATE_INACTIVE: AtomicU64 = AtomicU64::new(0);

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
    /// Indices into `nogood` of two predicates that were not satisfied when the nogood was last
    /// examined. While both are still unsatisfied, at least two predicates of the nogood are, so
    /// the nogood can neither conflict nor propagate and [`DeductionPropagator::propagate`] has
    /// nothing to do.
    ///
    /// The two indices are equal only for a nogood of one predicate, which has no such pair.
    watched: [usize; 2],
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

    /// Decide at notification time whether propagating is worth a trip through the queue.
    ///
    /// The engine's default enqueues on every satisfied predicate. While both watched predicates
    /// are still unsatisfied the nogood can do nothing, so the dispatch — queue, context, virtual
    /// call and the notification pass afterwards — is skipped here for the price of two lookups.
    fn notify_predicate_id_satisfied(
        &mut self,
        mut context: NotificationContext,
        _predicate_id: PredicateId,
    ) -> EnqueueDecision {
        // Opt-in, for measurement: deciding here changes the order in which propagators reach
        // the queue, hence which conflict is found, hence the proof.
        if !notify_skip_enabled() || !self.active {
            return EnqueueDecision::Enqueue;
        }

        let [first, second] = self.watched;
        let domains = context.domains();
        if first != second
            && domains.evaluate_predicate(self.nogood[first]) != Some(true)
            && domains.evaluate_predicate(self.nogood[second]) != Some(true)
        {
            return EnqueueDecision::Skip;
        }

        EnqueueDecision::Enqueue
    }

    fn propagate(&mut self, mut context: PropagationContext) -> PropagationStatusCP {
        // An escape hatch for measurement: with this set, propagation goes through the stateless
        // path, which is what this propagator did before it kept a watched pair.
        if watches_disabled() {
            return self.propagate_from_scratch(context);
        }

        let _ = PROPAGATE_CALLS.fetch_add(1, AtomicOrdering::Relaxed);

        if !self.active {
            for &predicate_id in self.ids.iter() {
                context.unregister_predicate(predicate_id);
            }

            let _ = PROPAGATE_INACTIVE.fetch_add(1, AtomicOrdering::Relaxed);
            return Ok(());
        }

        // While two distinct predicates of the nogood are unsatisfied, at least two are, so the
        // nogood can neither conflict nor propagate and there is nothing to do. Checking the
        // pair is O(1), where re-counting the whole nogood is not, and learned nogoods are long.
        let [first, second] = self.watched;
        if first != second
            && context.evaluate_predicate(self.nogood[first]) != Some(true)
            && context.evaluate_predicate(self.nogood[second]) != Some(true)
        {
            let _ = PROPAGATE_SHORTCUTS.fetch_add(1, AtomicOrdering::Relaxed);
            return Ok(());
        }

        // The pair no longer holds, so look for a new one. Whether that succeeds decides what
        // the nogood does, so the same scan answers both questions.
        let mut unsatisfied = [0_usize; 2];
        let mut num_unsatisfied = 0;

        for (index, &predicate) in self.nogood.iter().enumerate() {
            if context.evaluate_predicate(predicate) != Some(true) {
                if num_unsatisfied < 2 {
                    unsatisfied[num_unsatisfied] = index;
                }

                num_unsatisfied += 1;

                if num_unsatisfied == 2 {
                    self.watched = unsatisfied;
                    return Ok(());
                }
            }
        }

        match self.propagation_mode {
            DeductionPropagationMode::OnlyConflictDetection if num_unsatisfied == 0 => {
                Err(Conflict::Propagator(PropagatorConflict {
                    conjunction: self.nogood.iter().copied().collect(),
                    inference_code: self.inference_code.clone(),
                }))
            }

            DeductionPropagationMode::OnlyUnitPropagation if num_unsatisfied == 1 => {
                let unassigned_predicate = self.nogood[unsatisfied[0]];

                if context.evaluate_predicate(unassigned_predicate).is_none() {
                    let explanation = self
                        .nogood
                        .iter()
                        .copied()
                        .filter(|&predicate| predicate != unassigned_predicate)
                        .collect::<PropositionalConjunction>();

                    // This will never fail, as the predicate is known to be unassigned. So this
                    // propagator only returns explicit conflicts and never empty domain
                    // conflicts.
                    context.post(!unassigned_predicate, (explanation, &self.inference_code))?;
                }

                Ok(())
            }

            _ => Ok(()),
        }
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

        assert!(
            !(self.propagation_priority > Priority::VeryLow
                && matches!(
                    self.propagation_mode,
                    DeductionPropagationMode::OnlyUnitPropagation
                )
                && num_unassigned_predicates == 0),
            "It should not be possible for an unmarked deduction to unit propagate
            and cause a conflict, as a failure should have been declared by
            the `DeductionPropagator` that does conflict detection,
            which has a higher priority than its unit propagating counterpart."
        );
        match self.propagation_mode {
            DeductionPropagationMode::OnlyConflictDetection if num_unassigned_predicates == 0 => {
                return Err(Conflict::Propagator(PropagatorConflict {
                    conjunction: self.nogood.iter().copied().collect(),
                    inference_code: self.inference_code.clone(),
                }));
            }
            DeductionPropagationMode::OnlyUnitPropagation if num_unassigned_predicates == 1 => {
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
            _ => {}
        };
        Ok(())
    }

    fn priority(&self) -> Priority {
        self.propagation_priority
    }
}
