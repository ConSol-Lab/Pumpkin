#![allow(clippy::double_parens, reason = "originates inside the bitfield macro")]

use bitfield_struct::bitfield;
use pumpkin_checking::IntExt;
use pumpkin_core::predicate;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::proof::InferenceCode;
use pumpkin_core::propagation::ExplanationContext;
use pumpkin_core::propagation::LazyExplanation;
use pumpkin_core::propagation::ReadDomains;
use pumpkin_core::variables::IntegerVariable;

use super::shared::compute_quotient_bound_ext;
use super::shared::product_bound_ext;

/// Builds the lazy explanation for a propagation performed by
/// [`super::propagator::IntegerMultiplicationPropagator`], minimizing which domain bounds are
/// actually needed in the resulting reason.
#[derive(Clone, Debug)]
pub(super) struct IntegerMultiplicationExplainer {
    inference_code: InferenceCode,
    /// A re-usable buffer holding the explanation of the most recently explained propagation, at
    /// most four predicates.
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
                    PossiblyRedundantPredicate::lower(predicate![a >= a_min], a_min as i64),
                    PossiblyRedundantPredicate::upper(predicate![a <= a_max], a_max as i64),
                    PossiblyRedundantPredicate::lower(predicate![b >= b_min], b_min as i64),
                    PossiblyRedundantPredicate::upper(predicate![b <= b_max], b_max as i64),
                ],
                payload,
                |a_min, a_max, b_min, b_max| Some(product_bound_ext(a_min, a_max, b_min, b_max)),
            ),
            PropagatedBound::ALower | PropagatedBound::AUpper => minimize_reason(
                &mut self.reason_buffer,
                [
                    PossiblyRedundantPredicate::lower(predicate![c >= c_min], c_min as i64),
                    PossiblyRedundantPredicate::upper(predicate![c <= c_max], c_max as i64),
                    PossiblyRedundantPredicate::lower(predicate![b >= b_min], b_min as i64),
                    PossiblyRedundantPredicate::upper(predicate![b <= b_max], b_max as i64),
                ],
                payload,
                compute_quotient_bound_ext,
            ),
            PropagatedBound::BLower | PropagatedBound::BUpper => minimize_reason(
                &mut self.reason_buffer,
                [
                    PossiblyRedundantPredicate::lower(predicate![c >= c_min], c_min as i64),
                    PossiblyRedundantPredicate::upper(predicate![c <= c_max], c_max as i64),
                    PossiblyRedundantPredicate::lower(predicate![a >= a_min], a_min as i64),
                    PossiblyRedundantPredicate::upper(predicate![a <= a_max], a_max as i64),
                ],
                payload,
                compute_quotient_bound_ext,
            ),
        }

        LazyExplanation {
            predicates: self.reason_buffer.as_slice(),
            inference_code: self.inference_code.clone(),
        }
    }
}

/// A domain bound that may be used to justify a propagated value.
///
/// Combines the predicate that states it, and the value it relaxes to when [`minimize_reason`]
/// tests whether it can be dropped instead.
struct PossiblyRedundantPredicate {
    /// The predicate
    predicate: Predicate,
    /// The value for this bound (may not match the RHS in the predicate due to views).
    exact: i64,
    /// The value to relax to to test whether the predicate is redundant.
    ///
    /// This is either [`IntExt::NegativeInf`] or [`IntExt::PositiveInf`].
    relaxed: IntExt<i64>,
}

impl PossiblyRedundantPredicate {
    /// A lower bound `exact`, which relaxes to [`IntExt::NegativeInf`] when dropped.
    fn lower(predicate: Predicate, exact: i64) -> Self {
        PossiblyRedundantPredicate {
            predicate,
            exact,
            relaxed: IntExt::NegativeInf,
        }
    }

    /// An upper bound `exact`, which relaxes to [`IntExt::PositiveInf`] when dropped.
    fn upper(predicate: Predicate, exact: i64) -> Self {
        PossiblyRedundantPredicate {
            predicate,
            exact,
            relaxed: IntExt::PositiveInf,
        }
    }
}

/// Greedily drops bounds from the initial "use everything" reason for `bounds`.
///
/// In order, for each bound it is dropped if, when relaxing it, the `bound_fn` still justifies the
/// propagation we are explaining. Every bound that is deemed necessary is appended to `buffer`.
fn minimize_reason(
    buffer: &mut Vec<Predicate>,
    bounds: [PossiblyRedundantPredicate; 4],
    payload: MultiplicationPropagation,
    bound_fn: impl Fn(
        IntExt<i64>,
        IntExt<i64>,
        IntExt<i64>,
        IntExt<i64>,
    ) -> Option<(IntExt<i64>, IntExt<i64>)>,
) {
    let is_lower = payload.bound().is_lower();

    let mut values = bounds.each_ref().map(|bound| IntExt::Int(bound.exact));

    // Iterate through the values, and determine which ones can be removed. Results in a subset
    // minimal explanation, not a minimum explanation.
    for i in 0..4 {
        let exact = values[i];
        values[i] = bounds[i].relaxed;

        let propagated_value_with_relaxed_bound =
            match bound_fn(values[0], values[1], values[2], values[3]) {
                Some((lo, _)) if is_lower => lo,
                Some((_, hi)) => hi,
                None if is_lower => IntExt::NegativeInf,
                None => IntExt::PositiveInf,
            };

        let propagates_weaker = if is_lower {
            propagated_value_with_relaxed_bound >= IntExt::Int(payload.value() as i64)
        } else {
            propagated_value_with_relaxed_bound <= IntExt::Int(payload.value() as i64)
        };

        if !propagates_weaker {
            values[i] = exact;
            buffer.push(bounds[i].predicate);
        }
    }
}

/// Identifies which bound of which variable a lazily-explained propagation is for.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u8)]
pub(super) enum PropagatedBound {
    ALower = 0,
    AUpper = 1,
    BLower = 2,
    BUpper = 3,
    CLower = 4,
    CUpper = 5,
}

impl PropagatedBound {
    /// Whether this identifies a lower bound (`>=`) rather than an upper bound (`<=`).
    pub(super) const fn is_lower(self) -> bool {
        matches!(
            self,
            PropagatedBound::ALower | PropagatedBound::BLower | PropagatedBound::CLower
        )
    }

    const fn into_bits(self) -> u8 {
        self as _
    }

    const fn from_bits(value: u8) -> Self {
        match value {
            0 => PropagatedBound::ALower,
            1 => PropagatedBound::AUpper,
            2 => PropagatedBound::BLower,
            3 => PropagatedBound::BUpper,
            4 => PropagatedBound::CLower,
            _ => PropagatedBound::CUpper,
        }
    }
}

/// The payload carried by a [`pumpkin_core::engine::cp::reason::Reason::DynamicLazy`] reason for
/// the integer multiplication propagator: which propagation is being explained, and the value
/// that was propagated.
#[bitfield(u64)]
pub(super) struct MultiplicationPropagation {
    #[bits(8)]
    pub(super) bound: PropagatedBound,
    // The value has to be carried explicitly rather than read back off the trail predicate in
    // `IntegerMultiplicationExplainer::explain`: `a`, `b`, `c` may be `AffineView`s, and the
    // predicate that actually lands on the trail is stated in terms of the underlying `DomainId`,
    // not the view's own logical space that the rest of this propagator reasons in — its
    // right-hand side would generally not equal the value this propagator computed and posted.
    pub(super) value: i32,
    #[bits(24)]
    __: u32,
}
