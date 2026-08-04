use crate::basic_types::PredicateId;
use crate::engine::Reason;
use crate::predicates::Predicate;
use crate::proof::InferenceCode;
use crate::propagation::PropagationContext;
use crate::propagators::nogoods::NogoodPropagator;
use crate::propagators::nogoods::NogoodPropagatorStatistics;
use crate::state::Conflict;

/// A structure which allows the buffering of nogood propagations.
///
/// This is necessary since we require that two watchers can be placed before adding a nogood to
/// the database. If it is unit, then we cannot do this, and we buffer the propagations until we
/// can propagate.
#[derive(Debug, Clone, Default)]
pub(crate) struct PropagationBuffer {
    /// The unit propagations which are buffered.
    to_propagate: Vec<(Reason, Predicate)>,
    /// The extended nogood propagation which are buffered.
    to_propagate_extended: Vec<(Vec<PredicateId>, InferenceCode)>,
}

impl PropagationBuffer {
    pub(crate) fn buffer_unit_propagation(&mut self, reason: Reason, predicate: Predicate) {
        self.to_propagate.push((reason, predicate))
    }

    pub(crate) fn buffer_extended_nogood_propagation(
        &mut self,
        nogood: Vec<PredicateId>,
        inference_code: InferenceCode,
    ) {
        self.to_propagate_extended.push((nogood, inference_code))
    }

    pub(crate) fn propagate_buffer(
        &mut self,
        context: &mut PropagationContext,
        statistics: &mut NogoodPropagatorStatistics,
    ) -> Result<(), Conflict> {
        let result_unit = self
            .to_propagate
            .drain(..)
            .try_for_each(|(reason, predicate)| context.post(predicate, reason));

        if result_unit.is_err() {
            self.to_propagate_extended.clear();
            return result_unit.map_err(Into::into);
        }

        self.to_propagate_extended
            .drain(..)
            .try_for_each(|(nogood, inference_code)| {
                let propagated_domain =
                    context.get_predicate(*nogood.first().unwrap()).get_domain();
                NogoodPropagator::extended_nogood_propagation(
                    context,
                    &nogood,
                    propagated_domain,
                    &inference_code,
                    statistics,
                    None,
                )
            })
    }
}
