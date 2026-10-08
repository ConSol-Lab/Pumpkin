use pumpkin_checking::IntExt;
use pumpkin_checking::checkers::compute_quotient_bound_ext;
use pumpkin_checking::checkers::product_bound_ext;
use pumpkin_core::predicate;
use pumpkin_core::proof::InferenceCode;
use pumpkin_core::propagation::ExplanationContext;
use pumpkin_core::propagation::LazyExplanation;
use pumpkin_core::propagation::Priority;
use pumpkin_core::propagation::PropagationContext;
use pumpkin_core::propagation::Propagator;
use pumpkin_core::propagation::ReadDomains;
use pumpkin_core::state::PropagationStatusCP;
use pumpkin_core::variables::IntegerVariable;

use super::explainer::IntegerMultiplicationExplainer;
use crate::arithmetic::integer_multiplication::explainer::MultiplicationPropagation;
use crate::arithmetic::integer_multiplication::explainer::PropagatedBound;

/// A bounds(R)-consistent propagator for maintaining the constraint `a * b = c`.
///
/// # Bibliography
///
/// \[1\] C. Schulte & P. Stuckey, When Do Bounds and Domain Propagation Lead to the Same Search
/// Space? ACM Transactions of Programming Languages. 2025.
#[derive(Clone, Debug)]
pub struct IntegerMultiplicationPropagator<VA, VB, VC> {
    a: VA,
    b: VB,
    c: VC,
    explainer: IntegerMultiplicationExplainer,
}

impl<VA, VB, VC> IntegerMultiplicationPropagator<VA, VB, VC> {
    pub(super) fn new(a: VA, b: VB, c: VC, inference_code: InferenceCode) -> Self {
        let explainer = IntegerMultiplicationExplainer::new(inference_code);

        IntegerMultiplicationPropagator { a, b, c, explainer }
    }
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
        perform_propagation(context, &self.a, &self.b, &self.c)
    }

    fn lazy_explanation(&mut self, code: u64, context: ExplanationContext) -> LazyExplanation<'_> {
        let payload = MultiplicationPropagation::from_bits(code);
        self.explainer
            .explain(payload, context, &self.a, &self.b, &self.c)
    }
}

fn perform_propagation<VA: IntegerVariable, VB: IntegerVariable, VC: IntegerVariable>(
    mut context: PropagationContext,
    a: &VA,
    b: &VB,
    c: &VC,
) -> PropagationStatusCP {
    let a_min = context.lower_bound(a) as i64;
    let a_max = context.upper_bound(a) as i64;
    let b_min = context.lower_bound(b) as i64;
    let b_max = context.upper_bound(b) as i64;
    let c_min = context.lower_bound(c) as i64;
    let c_max = context.upper_bound(c) as i64;

    // c = a * b
    //
    // c \in [inf E .. sup E] where
    // E = {inf a * inf b, inf a * sup b, sup a * inf b, sup a * sup b}
    let (c_lo, c_hi) = product_bound(a_min, a_max, b_min, b_max);
    let c_lo = saturate_i64_to_i32(c_lo);
    let c_hi = saturate_i64_to_i32(c_hi);
    context.post(
        predicate![c >= c_lo],
        MultiplicationPropagation::new()
            .with_bound(PropagatedBound::CLower)
            .with_value(c_lo)
            .into_bits(),
    )?;
    context.post(
        predicate![c <= c_hi],
        MultiplicationPropagation::new()
            .with_bound(PropagatedBound::CUpper)
            .with_value(c_hi)
            .into_bits(),
    )?;

    // a = c / b
    propagate_quotient(
        &mut context,
        (c_min, c_max),
        (b_min, b_max),
        a,
        (PropagatedBound::ALower, PropagatedBound::AUpper),
    )?;

    // b = c / a
    propagate_quotient(
        &mut context,
        (c_min, c_max),
        (a_min, a_max),
        b,
        (PropagatedBound::BLower, PropagatedBound::BUpper),
    )?;

    Ok(())
}

/// Computes `[min E1 .. max E1]` where `E1` is the set of the four corner products of `[a_min ..
/// a_max] x [b_min .. b_max]`.
fn product_bound(a_min: i64, a_max: i64, b_min: i64, b_max: i64) -> (i64, i64) {
    let (lo, hi) = product_bound_ext(
        IntExt::Int(a_min),
        IntExt::Int(a_max),
        IntExt::Int(b_min),
        IntExt::Int(b_max),
    );

    (expect_finite(lo), expect_finite(hi))
}

/// Propagates the bounds of `target` in `target * denominator = numerator`.
fn propagate_quotient<VTarget: IntegerVariable>(
    context: &mut PropagationContext,
    numerator: (i64, i64),
    denominator: (i64, i64),
    target: &VTarget,
    codes: (PropagatedBound, PropagatedBound),
) -> PropagationStatusCP {
    let (num_min, num_max) = numerator;
    let (den_min, den_max) = denominator;
    let (lower_code, upper_code) = codes;

    let Some((lo, hi)) = compute_quotient_bound(num_min, num_max, den_min, den_max) else {
        return Ok(());
    };
    let lo = saturate_i64_to_i32(lo);
    let hi = saturate_i64_to_i32(hi);

    context.post(
        predicate![target >= lo],
        MultiplicationPropagation::new()
            .with_bound(lower_code)
            .with_value(lo)
            .into_bits(),
    )?;
    context.post(
        predicate![target <= hi],
        MultiplicationPropagation::new()
            .with_bound(upper_code)
            .with_value(hi)
            .into_bits(),
    )?;

    Ok(())
}

/// Computes the tightest range for `target` in `target * denominator = numerator`, or `None` if
/// no propagation is possible.
fn compute_quotient_bound(
    num_min: i64,
    num_max: i64,
    den_min: i64,
    den_max: i64,
) -> Option<(i64, i64)> {
    let (lo, hi) = compute_quotient_bound_ext(
        IntExt::Int(num_min),
        IntExt::Int(num_max),
        IntExt::Int(den_min),
        IntExt::Int(den_max),
    )?;

    Some((expect_finite(lo), expect_finite(hi)))
}

/// Panics if `value` is not [`IntExt::Int`]. Only used where the caller can prove the value must
/// be finite (e.g. because every input was finite).
fn expect_finite(value: IntExt<i64>) -> i64 {
    value
        .as_int()
        .expect("all inputs were finite, so the result must be finite too")
}

/// Clamps `value` into the range representable by `i32`.
fn saturate_i64_to_i32(value: i64) -> i32 {
    value.clamp(i32::MIN as i64, i32::MAX as i64) as i32
}
