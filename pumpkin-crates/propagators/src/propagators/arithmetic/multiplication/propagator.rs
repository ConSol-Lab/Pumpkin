use pumpkin_core::asserts::pumpkin_assert_simple;
use pumpkin_core::conjunction;
use pumpkin_core::predicate;
use pumpkin_core::proof::InferenceCode;
use pumpkin_core::propagation::Priority;
use pumpkin_core::propagation::PropagationContext;
use pumpkin_core::propagation::Propagator;
use pumpkin_core::propagation::ReadDomains;
use pumpkin_core::state::PropagationStatusCP;
use pumpkin_core::state::propagator_conflict;
use pumpkin_core::variables::IntegerVariable;

/// A propagator for maintaining the constraint `a * b = c`. The propagator
/// (currently) only propagates the signs of the variables, the case where a, b, c >= 0, and detects
/// a conflict if the variables are fixed.
#[derive(Clone, Debug)]
pub struct IntegerMultiplicationPropagator<VA, VB, VC> {
    pub(super) a: VA,
    pub(super) b: VB,
    pub(super) c: VC,
    pub(super) inference_code: InferenceCode,
}

impl<VA: 'static, VB: 'static, VC: 'static> Propagator
    for IntegerMultiplicationPropagator<VA, VB, VC>
where
    VA: IntegerVariable,
    VB: IntegerVariable,
    VC: IntegerVariable,
{
    fn priority(&self) -> Priority {
        Priority::High
    }

    fn name(&self) -> &str {
        "IntTimes"
    }

    fn propagate_from_scratch(&self, context: PropagationContext) -> PropagationStatusCP {
        perform_propagation(context, &self.a, &self.b, &self.c, &self.inference_code)
    }
}

fn perform_propagation<VA: IntegerVariable, VB: IntegerVariable, VC: IntegerVariable>(
    mut context: PropagationContext,
    a: &VA,
    b: &VB,
    c: &VC,
    inference_code: &InferenceCode,
) -> PropagationStatusCP {
    // First we propagate the signs
    propagate_signs(&mut context, a, b, c, inference_code)?;

    let a_min = context.lower_bound(a);
    let a_max = context.upper_bound(a);
    let b_min = context.lower_bound(b);
    let b_max = context.upper_bound(b);
    let c_min = context.lower_bound(c);
    let c_max = context.upper_bound(c);

    if a_min >= 0 && b_min >= 0 {
        let new_max_c = a_max.saturating_mul(b_max);
        let new_min_c = a_min.saturating_mul(b_min);

        // c is smaller than the maximum value that a * b can take
        //
        // We need the lower-bounds in the explanation as well because the reasoning does not
        // hold in the case of a negative lower-bound
        context.post(
            predicate![c <= new_max_c],
            (
                conjunction!([a >= 0] & [a <= a_max] & [b >= 0] & [b <= b_max]),
                inference_code,
            ),
        )?;

        // c is larger than the minimum value that a * b can take
        context.post(
            predicate![c >= new_min_c],
            (conjunction!([a >= a_min] & [b >= b_min]), inference_code),
        )?;
    }

    if b_min >= 0 && b_max >= 1 && c_min >= 1 {
        // a >= ceil(c.min / b.max)
        let bound = div_ceil_pos(c_min, b_max);
        context.post(
            predicate![a >= bound],
            (
                conjunction!([c >= c_min] & [b >= 0] & [b <= b_max]),
                inference_code,
            ),
        )?;
    }

    if b_min >= 1 && c_min >= 0 && c_max >= 1 {
        // a <= floor(c.max / b.min)
        let bound = c_max / b_min;
        context.post(
            predicate![a <= bound],
            (
                conjunction!([c >= 0] & [c <= c_max] & [b >= b_min]),
                inference_code,
            ),
        )?;
    }

    if a_min >= 1 && c_min >= 0 && c_max >= 1 {
        // b <= floor(c.max / a.min)
        let bound = c_max / a_min;
        context.post(
            predicate![b <= bound],
            (
                conjunction!([c >= 0] & [c <= c_max] & [a >= a_min]),
                inference_code,
            ),
        )?;
    }

    // b >= ceil(c.min / a.max)
    if a_min >= 0 && a_max >= 1 && c_min >= 1 {
        let bound = div_ceil_pos(c_min, a_max);

        context.post(
            predicate![b >= bound],
            (
                conjunction!([c >= c_min] & [a >= 0] & [a <= a_max]),
                inference_code,
            ),
        )?;
    }

    if let Some(fixed_a) = context.fixed_value(a)
        && let Some(fixed_b) = context.fixed_value(b)
        && let Some(fixed_c) = context.fixed_value(c)
        && (fixed_a * fixed_b) != fixed_c
    {
        // All variables are assigned but the resulting value is not correct, so we report a
        // conflict
        return propagator_conflict(
            conjunction!(
                [a == context.lower_bound(a)]
                    & [b == context.lower_bound(b)]
                    & [c == context.lower_bound(c)]
            ),
            inference_code,
        );
    }

    Ok(())
}

/// Propagates the signs of the variables, it performs the following propagations:
/// - Propagating based on positive bounds
///     - If a is positive and b is positive then c is positive
///     - If a is positive and c is positive then b is positive
///     - If b is positive and c is positive then a is positive
/// - Propagating based on negative bounds
///     - If a is negative and b is negative then c is positive
///     - If a is negative and c is negative then b is positive
///     - If b is negative and c is negative then b is positive
/// - Propagating based on mixed bounds
///     - Propagating c based on a and b
///         - If a is negative and b is positive then c is negative
///         - If a is positive and b is negative then c is negative
///     - Propagating b based on a and c
///         - If a is negative and c is positive then b is negative
///         - If a is positive and c is negative then b is negative
///     - Propagating a based on b and c
///         - If b is negative and c is positive then a is negative
///         - If b is positive and c is negative then a is negative
///
/// Note that this method does not propagate a value if 0 is in the domain as, for example, 0 * -3 =
/// 0 and 0 * 3 = 0 are both equally valid.
fn propagate_signs<VA: IntegerVariable, VB: IntegerVariable, VC: IntegerVariable>(
    context: &mut PropagationContext,
    a: &VA,
    b: &VB,
    c: &VC,
    inference_code: &InferenceCode,
) -> PropagationStatusCP {
    let a_min = context.lower_bound(a);
    let a_max = context.upper_bound(a);
    let b_min = context.lower_bound(b);
    let b_max = context.upper_bound(b);
    let c_min = context.lower_bound(c);
    let c_max = context.upper_bound(c);

    // Propagating based on positive bounds
    // a is positive and b is positive -> c is positive
    if a_min >= 0 && b_min >= 0 {
        context.post(
            predicate![c >= 0],
            (conjunction!([a >= 0] & [b >= 0]), inference_code),
        )?;
    }

    // a is positive and c is positive -> b is positive
    if a_min >= 1 && c_min >= 1 {
        context.post(
            predicate![b >= 1],
            (conjunction!([a >= 1] & [c >= 1]), inference_code),
        )?;
    }

    // b is positive and c is positive -> a is positive
    if b_min >= 1 && c_min >= 1 {
        context.post(
            predicate![a >= 1],
            (conjunction!([b >= 1] & [c >= 1]), inference_code),
        )?;
    }

    // Propagating based on negative bounds
    // a is negative and b is negative -> c is positive
    if a_max <= 0 && b_max <= 0 {
        context.post(
            predicate![c >= 0],
            (conjunction!([a <= 0] & [b <= 0]), inference_code),
        )?;
    }

    // a is negative and c is negative -> b is positive
    if a_max <= -1 && c_max <= -1 {
        context.post(
            predicate![b >= 1],
            (conjunction!([a <= -1] & [c <= -1]), inference_code),
        )?;
    }

    // b is negative and c is negative -> a is positive
    if b_max <= -1 && c_max <= -1 {
        context.post(
            predicate![a >= 1],
            (conjunction!([b <= -1] & [c <= -1]), inference_code),
        )?;
    }

    // Propagating based on mixed bounds (i.e. one positive and one negative)
    // Propagating c based on a and b
    // a is negative and b is positive -> c is negative
    if a_max <= 0 && b_min >= 0 {
        context.post(
            predicate![c <= 0],
            (conjunction!([a <= 0] & [b >= 0]), inference_code),
        )?;
    }

    // a is positive and b is negative -> c is negative
    if a_min >= 0 && b_max <= 0 {
        context.post(
            predicate![c <= 0],
            (conjunction!([a >= 0] & [b <= 0]), inference_code),
        )?;
    }

    // Propagating b based on a and c
    // a is negative and c is positive -> b is negative
    if a_max <= -1 && c_min >= 1 {
        context.post(
            predicate![b <= -1],
            (conjunction!([a <= -1] & [c >= 1]), inference_code),
        )?;
    }

    // a is positive and c is negative -> b is negative
    if a_min >= 1 && c_max <= -1 {
        context.post(
            predicate![b <= -1],
            (conjunction!([a >= 1] & [c <= -1]), inference_code),
        )?;
    }

    // Propagating a based on b and c
    // b is negative and c is positive -> a is negative
    if b_max <= -1 && c_min >= 1 {
        context.post(
            predicate![a <= -1],
            (conjunction!([b <= -1] & [c >= 1]), inference_code),
        )?;
    }

    // b is positive and c is negative -> a is negative
    if b_min >= 1 && c_max <= -1 {
        context.post(
            predicate![a <= -1],
            (conjunction!([b >= 1] & [c <= -1]), inference_code),
        )?;
    }

    Ok(())
}

/// Compute `ceil(numerator / denominator)`.
///
/// Assumes `numerator, denominator > 0`.
#[inline]
fn div_ceil_pos(numerator: i32, denominator: i32) -> i32 {
    pumpkin_assert_simple!(
        numerator > 0 && denominator > 0,
        "Either the numerator {numerator} was non-positive or the denominator {denominator} was non-positive"
    );
    numerator / denominator + (numerator % denominator).signum()
}
