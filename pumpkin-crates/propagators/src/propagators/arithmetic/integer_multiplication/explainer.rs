use pumpkin_checking::IntExt;
use pumpkin_core::predicate;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::proof::InferenceCode;
use pumpkin_core::propagation::ExplanationContext;
use pumpkin_core::propagation::LazyExplanation;
use pumpkin_core::propagation::ReadDomains;
use pumpkin_core::variables::IntegerVariable;

use super::shared::MultiplicationPropagation;
use super::shared::PropagatedBound;
use super::shared::compute_quotient_bound_ext;
use super::shared::product_bound_ext;

/// Builds the lazy explanation for a propagation performed by
/// [`super::propagator::IntegerMultiplicationPropagator`], minimizing which domain bounds are
/// actually cited in the resulting reason.
#[derive(Clone, Debug)]
pub(super) struct IntegerMultiplicationExplainer {
    inference_code: InferenceCode,
    /// A re-usable buffer holding the explanation of the most recently explained propagation, at
    /// most four predicates.
    // Owned here, rather than a local in `explain`, because that method returns a slice borrowed
    // from it.
    reason_buffer: Vec<Predicate>,
}

impl IntegerMultiplicationExplainer {
    pub(super) fn new(inference_code: InferenceCode) -> Self {
        IntegerMultiplicationExplainer {
            inference_code,
            reason_buffer: Vec::new(),
        }
    }

    /// Explains the propagation identified by `payload`, given the variables it was propagated
    /// over.
    pub(super) fn explain<VA: IntegerVariable, VB: IntegerVariable, VC: IntegerVariable>(
        &mut self,
        payload: MultiplicationPropagation,
        context: ExplanationContext,
        a: &VA,
        b: &VB,
        c: &VC,
    ) -> LazyExplanation<'_> {
        let bound = payload.bound();
        let target = payload.value() as i64;

        let trail_position = context.get_trail_position();
        let a_min = context.lower_bound_at_trail_position(a, trail_position);
        let a_max = context.upper_bound_at_trail_position(a, trail_position);
        let b_min = context.lower_bound_at_trail_position(b, trail_position);
        let b_max = context.upper_bound_at_trail_position(b, trail_position);
        let c_min = context.lower_bound_at_trail_position(c, trail_position);
        let c_max = context.upper_bound_at_trail_position(c, trail_position);

        self.reason_buffer.clear();
        match bound {
            PropagatedBound::CLower | PropagatedBound::CUpper => minimize_reason(
                &mut self.reason_buffer,
                [
                    CitableBound::lower(predicate![a >= a_min], a_min as i64),
                    CitableBound::upper(predicate![a <= a_max], a_max as i64),
                    CitableBound::lower(predicate![b >= b_min], b_min as i64),
                    CitableBound::upper(predicate![b <= b_max], b_max as i64),
                ],
                target,
                bound.is_lower(),
                |a_min, a_max, b_min, b_max| Some(product_bound_ext(a_min, a_max, b_min, b_max)),
            ),
            PropagatedBound::ALower | PropagatedBound::AUpper => minimize_reason(
                &mut self.reason_buffer,
                [
                    CitableBound::lower(predicate![c >= c_min], c_min as i64),
                    CitableBound::upper(predicate![c <= c_max], c_max as i64),
                    CitableBound::lower(predicate![b >= b_min], b_min as i64),
                    CitableBound::upper(predicate![b <= b_max], b_max as i64),
                ],
                target,
                bound.is_lower(),
                compute_quotient_bound_ext,
            ),
            PropagatedBound::BLower | PropagatedBound::BUpper => minimize_reason(
                &mut self.reason_buffer,
                [
                    CitableBound::lower(predicate![c >= c_min], c_min as i64),
                    CitableBound::upper(predicate![c <= c_max], c_max as i64),
                    CitableBound::lower(predicate![a >= a_min], a_min as i64),
                    CitableBound::upper(predicate![a <= a_max], a_max as i64),
                ],
                target,
                bound.is_lower(),
                compute_quotient_bound_ext,
            ),
        }

        LazyExplanation {
            predicates: self.reason_buffer.as_slice(),
            inference_code: self.inference_code.clone(),
        }
    }
}

/// A domain bound that may be cited to justify a propagated value: the predicate that states it,
/// and the value it relaxes to when [`minimize_reason`] tests whether it can be dropped instead.
struct CitableBound {
    predicate: Predicate,
    exact: i64,
    relaxed: IntExt<i64>,
}

impl CitableBound {
    /// A lower bound `exact`, which relaxes to [`IntExt::NegativeInf`] when dropped.
    fn lower(predicate: Predicate, exact: i64) -> Self {
        CitableBound {
            predicate,
            exact,
            relaxed: IntExt::NegativeInf,
        }
    }

    /// An upper bound `exact`, which relaxes to [`IntExt::PositiveInf`] when dropped.
    fn upper(predicate: Predicate, exact: i64) -> Self {
        CitableBound {
            predicate,
            exact,
            relaxed: IntExt::PositiveInf,
        }
    }
}

/// Greedily drops bounds from the initial "cite everything" reason for `bounds`, keeping the drop
/// only if `bound_fn`, recomputed with the relaxed value, still justifies `target` (i.e. is still
/// `>= target` when `is_lower`, or `<= target` otherwise). Appends the predicates of the bounds
/// that remain necessary to `buffer`.
fn minimize_reason(
    buffer: &mut Vec<Predicate>,
    bounds: [CitableBound; 4],
    target: i64,
    is_lower: bool,
    bound_fn: impl Fn(
        IntExt<i64>,
        IntExt<i64>,
        IntExt<i64>,
        IntExt<i64>,
    ) -> Option<(IntExt<i64>, IntExt<i64>)>,
) {
    let is_sufficient = |values: [IntExt<i64>; 4]| {
        let value = match bound_fn(values[0], values[1], values[2], values[3]) {
            Some((lo, _)) if is_lower => lo,
            Some((_, hi)) => hi,
            None if is_lower => IntExt::NegativeInf,
            None => IntExt::PositiveInf,
        };

        if is_lower {
            value >= IntExt::Int(target)
        } else {
            value <= IntExt::Int(target)
        }
    };

    let mut values = bounds.each_ref().map(|bound| IntExt::Int(bound.exact));
    let mut kept = [true; 4];

    // Relaxing a bound can only loosen the value `bound_fn` computes, so sufficiency is monotone
    // in the cited set: a single greedy pass is enough to reach a sound, irredundant (no further
    // bound can be dropped) reason, though not necessarily the smallest one possible.
    for i in 0..4 {
        let exact = values[i];
        values[i] = bounds[i].relaxed;
        if is_sufficient(values) {
            kept[i] = false;
        } else {
            values[i] = exact;
        }
    }

    buffer.extend(
        bounds
            .into_iter()
            .zip(kept)
            .filter_map(|(bound, kept)| kept.then_some(bound.predicate)),
    );
}
