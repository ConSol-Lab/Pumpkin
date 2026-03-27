use crate::engine::reason::ReasonRef;
use crate::predicates::Predicate;
use crate::predicates::PropositionalConjunction;
use crate::proof::InferenceCode;
use crate::propagation::ExplanationContext;
use crate::state::CurrentNogood;
use crate::state::State;
use crate::variables::DomainId;

/// The result of invoking a constraint programming propagator. The propagation can either succeed
/// or identify a conflict. The necessary conditions for the conflict must be captured in the error
/// variant, i.e. a propositional conjunction.
pub type PropagationStatusCP = Result<(), Conflict>;

/// Convenience function to create [`PropagationStatusCP`] with a [`PropagatorConflict`].
pub fn propagator_conflict(
    conjunction: PropositionalConjunction,
    inference_code: &InferenceCode,
) -> PropagationStatusCP {
    Err(Conflict::Propagator(PropagatorConflict {
        conjunction,
        inference_code: inference_code.clone(),
    }))
}

/// Information concerning the conflict returned by [`State::propagate_to_fixed_point`].
///
/// Two (related) conflicts can happen:
/// 1) a propagator explicitly detects a conflict.
/// 2) a propagator post a domain change that results in a variable having an empty domain.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Conflict {
    /// A conflict raised explicitly by a propagator.
    Propagator(PropagatorConflict),
    /// A conflict caused by an empty domain for a variable occurring.
    EmptyDomain(EmptyDomainConflict),
}

impl From<EmptyDomainConflict> for Conflict {
    fn from(value: EmptyDomainConflict) -> Self {
        Conflict::EmptyDomain(value)
    }
}

impl From<PropagatorConflict> for Conflict {
    fn from(value: PropagatorConflict) -> Self {
        Conflict::Propagator(value)
    }
}

/// A conflict because a domain became empty.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct EmptyDomainConflict {
    /// The predicate that caused a domain to become empty.
    pub trigger_predicate: Predicate,
    /// The [`InferenceCode`] that accompanies triggered the conflict.
    ///
    /// If the empty domain is not triggered by a propagation, this is [`None`].
    pub trigger_inference_code: Option<InferenceCode>,

    /// The reason for [`EmptyDomainConflict::trigger_predicate`] to be true.
    ///
    /// If the empty domain is not triggered by a propagation, this is [`None`].
    pub(crate) trigger_reason: Option<ReasonRef>,
}

impl EmptyDomainConflict {
    /// The domain that became empty.
    pub fn domain(&self) -> DomainId {
        self.trigger_predicate.get_domain()
    }

    /// Returns the reason for the [`EmptyDomainConflict::trigger_predicate`] being propagated to
    /// true while it is already false in the [`State`].
    pub fn get_reason(
        &self,
        state: &mut State,
        reason_buffer: &mut (impl Extend<Predicate> + AsRef<[Predicate]>),
        current_nogood: CurrentNogood,
    ) {
        if let Some(reason_ref) = self.trigger_reason {
            let _ = state.reason_store.get_or_compute(
                reason_ref,
                ExplanationContext::new(
                    &state.assignments,
                    current_nogood,
                    state.trail_len(),
                    &mut state.notification_engine,
                ),
                &mut state.propagators,
                reason_buffer,
            );
        }
    }
}

/// A conflict stated by a propagator. A propagator that identifies a conflict that is _not_ an
/// empty domain, describes that conflict with this type.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PropagatorConflict {
    /// The conjunction that describes the infeasible partial assignment.
    pub conjunction: PropositionalConjunction,
    /// The inference code that identified the conflict.
    pub inference_code: InferenceCode,
}
