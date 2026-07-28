//! Numeric helpers and the propagation/explanation payload encoding shared by
//! [`super::propagator`], [`super::explainer`], and [`super::checker`].
#![allow(clippy::double_parens, reason = "originates inside the bitfield macro")]

use bitfield_struct::bitfield;
use pumpkin_checking::IntExt;

/// Computes `[min E1 .. max E1]` where `E1` is the set of the four corner products of `[a_min ..
/// a_max] x [b_min .. b_max]`, generalized to [`IntExt<i64>`] operands.
pub(super) fn product_bound_ext(
    a_min: IntExt<i64>,
    a_max: IntExt<i64>,
    b_min: IntExt<i64>,
    b_max: IntExt<i64>,
) -> (IntExt<i64>, IntExt<i64>) {
    // The propagator's actual output is always computed from finite domain bounds, and is always
    // itself finite. Infinities only ever arise from `minimize_reason`, which relaxes individual
    // domain bounds to determine whether they are actually necessary to justify a propagated
    // value; a relaxed bound that turns the recomputed value into (or through) an infinity is one
    // that cannot be dropped from the reason.
    let corners = [a_min * b_min, a_min * b_max, a_max * b_min, a_max * b_max];

    (
        corners.into_iter().min().expect("corners is non-empty"),
        corners.into_iter().max().expect("corners is non-empty"),
    )
}

/// Computes the tightest range for `target` in `target * denominator = numerator`, generalized to
/// [`IntExt<i64>`] operands, or `None` if no propagation is possible.
pub(super) fn compute_quotient_bound_ext(
    num_min: IntExt<i64>,
    num_max: IntExt<i64>,
    den_min: IntExt<i64>,
    den_max: IntExt<i64>,
) -> Option<(IntExt<i64>, IntExt<i64>)> {
    let zero = IntExt::Int(0);
    let den_straddles_zero = den_min <= zero && den_max >= zero;
    let num_straddles_zero = num_min <= zero && num_max >= zero;

    if den_straddles_zero && num_straddles_zero {
        return None;
    }

    if !den_straddles_zero {
        return Some(quotient_bound_ext(num_min, num_max, den_min, den_max));
    }

    let branch_pos = (den_max >= IntExt::Int(1))
        .then(|| quotient_bound_ext(num_min, num_max, IntExt::Int(1), den_max));
    let branch_neg = (den_min <= IntExt::Int(-1))
        .then(|| quotient_bound_ext(num_min, num_max, den_min, IntExt::Int(-1)));

    match (branch_pos, branch_neg) {
        (Some((lo_pos, hi_pos)), Some((lo_neg, hi_neg))) => {
            Some((lo_pos.min(lo_neg), hi_pos.max(hi_neg)))
        }
        (Some(bound), None) | (None, Some(bound)) => Some(bound),
        // This means `denominator` is fixed to exactly zero, which would already have forced
        // `numerator`'s bound to include zero (and hence conflicted, since `numerator` here
        // excludes it) via the `c = a * b` propagation computed earlier from the same snapshot.
        (None, None) => unreachable!(),
    }
}

/// Computes `[ceil(inf E2) .. floor(sup E2)]` where `E2` is the set of the four corner quotients
/// of `[num_min .. num_max] / [den_min .. den_max]`, generalized to [`IntExt<i64>`] operands.
/// Assumes `[den_min .. den_max]` does not contain zero.
fn quotient_bound_ext(
    num_min: IntExt<i64>,
    num_max: IntExt<i64>,
    den_min: IntExt<i64>,
    den_max: IntExt<i64>,
) -> (IntExt<i64>, IntExt<i64>) {
    // A corner division that is indeterminate (infinity divided by infinity) is treated
    // conservatively rather than propagated as an error: `None` becomes `NegativeInf` for the
    // `ceil`/min aggregation and `PositiveInf` for the `floor`/max aggregation, so that an
    // indeterminate corner can never cause `minimize_reason` to *overestimate* how tight the true
    // bound is. This only ever costs some generality, deep inside an already-heavily-relaxed
    // reason — never soundness.
    let ceil = |n: IntExt<i64>, d: IntExt<i64>| n.div_ceil(d).unwrap_or(IntExt::NegativeInf);
    let floor = |n: IntExt<i64>, d: IntExt<i64>| n.div_floor(d).unwrap_or(IntExt::PositiveInf);

    let inf_e2 = ceil(num_min, den_min)
        .min(ceil(num_min, den_max))
        .min(ceil(num_max, den_min))
        .min(ceil(num_max, den_max));

    let sup_e2 = floor(num_min, den_min)
        .max(floor(num_min, den_max))
        .max(floor(num_max, den_min))
        .max(floor(num_max, den_max));

    (inf_e2, sup_e2)
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
