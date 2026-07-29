use pumpkin_checking::IntExt;

/// Performs interval arithmetic multiplication.
///
/// Returns [a_min .. a_max] * [b_min .. b_max], generalized to [`IntExt`] operands.
pub(super) fn product_bound_ext(
    a_min: IntExt<i64>,
    a_max: IntExt<i64>,
    b_min: IntExt<i64>,
    b_max: IntExt<i64>,
) -> (IntExt<i64>, IntExt<i64>) {
    let corners = [a_min * b_min, a_min * b_max, a_max * b_min, a_max * b_max];

    (
        corners.into_iter().min().expect("corners is non-empty"),
        corners.into_iter().max().expect("corners is non-empty"),
    )
}

/// Computes the tightest range for `target` in `target * denominator = numerator`.
///
/// If no propagation is possible, returns [`None`].
///
/// Generalized to [`IntExt<i64>`] operands.
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
///
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
    // bound is.

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
