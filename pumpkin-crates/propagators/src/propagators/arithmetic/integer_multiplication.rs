#![allow(clippy::double_parens, reason = "originates inside the bitfield macro")]
use bitfield_struct::bitfield;
use pumpkin_checking::AtomicConstraint;
use pumpkin_checking::CheckerVariable;
use pumpkin_checking::InferenceChecker;
use pumpkin_checking::IntExt;
use pumpkin_checking::VariableState;
use pumpkin_core::declare_inference_label;
use pumpkin_core::predicate;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::proof::ConstraintTag;
use pumpkin_core::proof::InferenceCode;
use pumpkin_core::propagation::DomainEvents;
use pumpkin_core::propagation::EventsToRegister;
use pumpkin_core::propagation::ExplanationContext;
use pumpkin_core::propagation::InferenceCheckers;
use pumpkin_core::propagation::LazyExplanation;
use pumpkin_core::propagation::LocalId;
use pumpkin_core::propagation::Priority;
use pumpkin_core::propagation::PropagationContext;
use pumpkin_core::propagation::Propagator;
use pumpkin_core::propagation::PropagatorConstructor;
use pumpkin_core::propagation::PropagatorConstructorContext;
use pumpkin_core::propagation::ReadDomains;
use pumpkin_core::state::PropagationStatusCP;
use pumpkin_core::variables::IntegerVariable;

declare_inference_label!(IntegerMultiplication);

/// The [`PropagatorConstructor`] for [`IntegerMultiplicationPropagator`].
#[derive(Clone, Debug)]
pub struct IntegerMultiplicationArgs<VA, VB, VC> {
    pub a: VA,
    pub b: VB,
    pub c: VC,
    pub constraint_tag: ConstraintTag,
}

impl<VA, VB, VC> PropagatorConstructor for IntegerMultiplicationArgs<VA, VB, VC>
where
    VA: IntegerVariable + 'static,
    VB: IntegerVariable + 'static,
    VC: IntegerVariable + 'static,
{
    type PropagatorImpl = IntegerMultiplicationPropagator<VA, VB, VC>;

    fn add_inference_checkers(&self, mut checkers: InferenceCheckers<'_>) {
        checkers.add_inference_checker(
            InferenceCode::new(self.constraint_tag, IntegerMultiplication),
            Box::new(IntegerMultiplicationChecker {
                a: self.a.clone(),
                b: self.b.clone(),
                c: self.c.clone(),
            }),
        );
    }

    fn create(self, _: PropagatorConstructorContext) -> (EventsToRegister, Self::PropagatorImpl) {
        let IntegerMultiplicationArgs {
            a,
            b,
            c,
            constraint_tag,
        } = self;

        let registration = EventsToRegister::builder()
            .add(&a, DomainEvents::ANY_INT, ID_A)
            .add(&b, DomainEvents::ANY_INT, ID_B)
            .add(&c, DomainEvents::ANY_INT, ID_C)
            .build();

        let propagator = IntegerMultiplicationPropagator {
            a,
            b,
            c,
            inference_code: InferenceCode::new(constraint_tag, IntegerMultiplication),
            reason_buffer: Vec::new(),
        };

        (registration, propagator)
    }
}

/// A propagator for maintaining the constraint `a * b = c`.
///
/// The propagator is bounds(R)-consistent, following Schulte & Stuckey, "When Do Bounds and
/// Domain Propagation Lead to the Same Search Space?" (ACM TOPLAS 27(3), 2005), §2.3.
///
/// Explanations are computed lazily (see [`Propagator::lazy_explanation`]), and are minimized on
/// demand: a domain bound is only cited in a reason if it is actually necessary to justify the
/// propagated value, determined by [`minimize_reason`].
#[derive(Clone, Debug)]
pub struct IntegerMultiplicationPropagator<VA, VB, VC> {
    a: VA,
    b: VB,
    c: VC,
    inference_code: InferenceCode,
    /// A re-usable buffer holding the explanation of the most recently explained propagation, at
    /// most four predicates.
    // Owned here, rather than a local in `lazy_explanation`, because that method returns a slice
    // borrowed from it.
    reason_buffer: Vec<Predicate>,
}

const ID_A: LocalId = LocalId::from(0);
const ID_B: LocalId = LocalId::from(1);
const ID_C: LocalId = LocalId::from(2);

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
        let bound = payload.bound();
        let target = payload.value() as i64;

        let trail_position = context.get_trail_position();
        let a_min = context.lower_bound_at_trail_position(&self.a, trail_position);
        let a_max = context.upper_bound_at_trail_position(&self.a, trail_position);
        let b_min = context.lower_bound_at_trail_position(&self.b, trail_position);
        let b_max = context.upper_bound_at_trail_position(&self.b, trail_position);
        let c_min = context.lower_bound_at_trail_position(&self.c, trail_position);
        let c_max = context.upper_bound_at_trail_position(&self.c, trail_position);

        self.reason_buffer.clear();
        match bound {
            PropagatedBound::CLower | PropagatedBound::CUpper => minimize_reason(
                &mut self.reason_buffer,
                [
                    CitableBound::lower(predicate![self.a >= a_min], a_min as i64),
                    CitableBound::upper(predicate![self.a <= a_max], a_max as i64),
                    CitableBound::lower(predicate![self.b >= b_min], b_min as i64),
                    CitableBound::upper(predicate![self.b <= b_max], b_max as i64),
                ],
                target,
                bound.is_lower(),
                |a_min, a_max, b_min, b_max| Some(product_bound_ext(a_min, a_max, b_min, b_max)),
            ),
            PropagatedBound::ALower | PropagatedBound::AUpper => minimize_reason(
                &mut self.reason_buffer,
                [
                    CitableBound::lower(predicate![self.c >= c_min], c_min as i64),
                    CitableBound::upper(predicate![self.c <= c_max], c_max as i64),
                    CitableBound::lower(predicate![self.b >= b_min], b_min as i64),
                    CitableBound::upper(predicate![self.b <= b_max], b_max as i64),
                ],
                target,
                bound.is_lower(),
                compute_quotient_bound_ext,
            ),
            PropagatedBound::BLower | PropagatedBound::BUpper => minimize_reason(
                &mut self.reason_buffer,
                [
                    CitableBound::lower(predicate![self.c >= c_min], c_min as i64),
                    CitableBound::upper(predicate![self.c <= c_max], c_max as i64),
                    CitableBound::lower(predicate![self.a >= a_min], a_min as i64),
                    CitableBound::upper(predicate![self.a <= a_max], a_max as i64),
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

/// The [`IntExt<i64>`]-generalized form of [`product_bound`]; see its documentation.
fn product_bound_ext(
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

/// The [`IntExt<i64>`]-generalized form of [`compute_quotient_bound`]; see its documentation.
fn compute_quotient_bound_ext(
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
enum PropagatedBound {
    ALower = 0,
    AUpper = 1,
    BLower = 2,
    BUpper = 3,
    CLower = 4,
    CUpper = 5,
}

impl PropagatedBound {
    /// Whether this identifies a lower bound (`>=`) rather than an upper bound (`<=`).
    const fn is_lower(self) -> bool {
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
/// this propagator: which propagation is being explained, and the value that was propagated.
#[bitfield(u64)]
struct MultiplicationPropagation {
    #[bits(8)]
    bound: PropagatedBound,
    // The value has to be carried explicitly rather than read back off the trail predicate in
    // `Propagator::lazy_explanation`: `a`, `b`, `c` may be `AffineView`s, and the predicate that
    // actually lands on the trail is stated in terms of the underlying `DomainId`, not the view's
    // own logical space that the rest of this propagator reasons in — its right-hand side would
    // generally not equal the value this propagator computed and posted.
    value: i32,
    #[bits(24)]
    __: u32,
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

/// Clamps `value` into the range representable by `i32`.
fn saturate_i64_to_i32(value: i64) -> i32 {
    value.clamp(i32::MIN as i64, i32::MAX as i64) as i32
}

/// Verifies that a claimed inference for `a * b = c` is actually implied by its premises.
#[derive(Clone, Debug)]
pub struct IntegerMultiplicationChecker<VA, VB, VC> {
    pub a: VA,
    pub b: VB,
    pub c: VC,
}

impl<VA, VB, VC, Atomic> InferenceChecker<Atomic> for IntegerMultiplicationChecker<VA, VB, VC>
where
    Atomic: AtomicConstraint,
    VA: CheckerVariable<Atomic>,
    VB: CheckerVariable<Atomic>,
    VC: CheckerVariable<Atomic>,
{
    fn check(
        &self,
        state: VariableState<Atomic>,
        _premises: &[Atomic],
        consequent: Option<&Atomic>,
    ) -> bool {
        let a_min = self.a.induced_lower_bound(&state).into();
        let a_max = self.a.induced_upper_bound(&state).into();
        let b_min = self.b.induced_lower_bound(&state).into();
        let b_max = self.b.induced_upper_bound(&state).into();
        let c_min = self.c.induced_lower_bound(&state).into();
        let c_max = self.c.induced_upper_bound(&state).into();

        let check_c = || {
            let (lo, hi) = product_bound_ext(a_min, a_max, b_min, b_max);
            is_disjoint(lo, hi, c_min, c_max)
        };
        let check_a = || {
            compute_quotient_bound_ext(c_min, c_max, b_min, b_max)
                .is_some_and(|(lo, hi)| is_disjoint(lo, hi, a_min, a_max))
        };
        let check_b = || {
            compute_quotient_bound_ext(c_min, c_max, a_min, a_max)
                .is_some_and(|(lo, hi)| is_disjoint(lo, hi, b_min, b_max))
        };

        match consequent {
            Some(atomic) if self.c.does_atomic_constrain_self(atomic) => check_c(),
            Some(atomic) if self.a.does_atomic_constrain_self(atomic) => check_a(),
            Some(atomic) if self.b.does_atomic_constrain_self(atomic) => check_b(),
            Some(_) => false,
            None => check_c() || check_a() || check_b(),
        }
    }
}

/// Returns whether `[induced_lo, induced_hi]` shares no value with `[lo, hi]`.
fn is_disjoint(
    lo: IntExt<i64>,
    hi: IntExt<i64>,
    induced_lo: IntExt<i64>,
    induced_hi: IntExt<i64>,
) -> bool {
    induced_hi < lo || induced_lo > hi
}

/// Panics if `value` is not [`IntExt::Int`]. Only used where the caller can prove the value must
/// be finite (e.g. because every input was finite).
fn expect_finite(value: IntExt<i64>) -> i64 {
    value
        .as_int()
        .expect("all inputs were finite, so the result must be finite too")
}

#[cfg(test)]
mod tests {
    use pumpkin_core::conjunction;
    use pumpkin_core::predicate;
    use pumpkin_core::predicates::Predicate;
    use pumpkin_core::predicates::PropositionalConjunction;
    use pumpkin_core::propagation::CurrentNogood;
    use pumpkin_core::state::State;
    use pumpkin_core::variables::TransformableVariable;

    use super::*;
    use crate::StateExt;

    fn reason_for(state: &mut State, predicate: Predicate) -> PropositionalConjunction {
        let mut reason_buffer: Vec<Predicate> = vec![];
        let _ = state.get_propagation_reason(predicate, &mut reason_buffer, CurrentNogood::empty());
        reason_buffer.into()
    }

    fn new_propagator(
        state: &mut State,
        a: pumpkin_core::variables::DomainId,
        b: pumpkin_core::variables::DomainId,
        c: pumpkin_core::variables::DomainId,
    ) {
        let constraint_tag = state.new_constraint_tag();
        let _ = state.add_propagator(IntegerMultiplicationArgs {
            a,
            b,
            c,
            constraint_tag,
        });
    }

    #[test]
    fn both_positive_propagates_bounds_c() {
        let mut state = State::default();
        let a = state.new_interval_variable(1, 3, None);
        let b = state.new_interval_variable(0, 4, None);
        let c = state.new_interval_variable(-10, 20, None);
        new_propagator(&mut state, a, b, c);

        state.propagate_to_fixed_point().expect("no empty domains");

        state.assert_bounds(a, 1, 3);
        state.assert_bounds(b, 0, 4);
        state.assert_bounds(c, 0, 12);

        // Both factors are non-negative, so the lower bound only needs each factor's own
        // non-negativity, not the full box.
        let reason = reason_for(&mut state, predicate![c >= 0]);
        assert_eq!(conjunction!([a >= 1] & [b >= 0]), reason);

        let reason = reason_for(&mut state, predicate![c <= 12]);
        assert_eq!(conjunction!([a <= 3] & [b >= 0] & [b <= 4]), reason);
    }

    #[test]
    fn propagates_correctly_through_a_negative_affine_view() {
        // Same scenario as `both_positive_propagates_bounds_c`, but `c` is a view `-c_underlying`
        // rather than a plain `DomainId`. The predicate that actually lands on the trail is
        // therefore stated in terms of `c_underlying` (with the inequality direction flipped),
        // and its right-hand side does not equal the value this propagator computed for `c`
        // itself: `lazy_explanation` must not assume the two coincide.
        let mut state = State::default();
        let a = state.new_interval_variable(1, 3, None);
        let b = state.new_interval_variable(0, 4, None);
        let c_underlying = state.new_interval_variable(-20, 10, None);
        let c = c_underlying.scaled(-1);

        let constraint_tag = state.new_constraint_tag();
        let _ = state.add_propagator(IntegerMultiplicationArgs {
            a,
            b,
            c,
            constraint_tag,
        });

        state.propagate_to_fixed_point().expect("no empty domains");

        assert_eq!(state.lower_bound(c), 0);
        assert_eq!(state.upper_bound(c), 12);

        let reason = reason_for(&mut state, predicate![c >= 0]);
        assert_eq!(conjunction!([a >= 1] & [b >= 0]), reason);

        let reason = reason_for(&mut state, predicate![c <= 12]);
        assert_eq!(conjunction!([a <= 3] & [b >= 0] & [b <= 4]), reason);
    }

    #[test]
    fn both_negative_propagates_bounds_c() {
        let mut state = State::default();
        let a = state.new_interval_variable(-5, -2, None);
        let b = state.new_interval_variable(-4, -1, None);
        let c = state.new_interval_variable(-100, 100, None);
        new_propagator(&mut state, a, b, c);

        state.propagate_to_fixed_point().expect("no empty domains");

        // Corners: (-5)(-4)=20, (-5)(-1)=5, (-2)(-4)=8, (-2)(-1)=2.
        state.assert_bounds(c, 2, 20);
    }

    #[test]
    fn mixed_sign_propagates_bounds_c() {
        let mut state = State::default();
        let a = state.new_interval_variable(2, 5, None);
        let b = state.new_interval_variable(-4, -1, None);
        let c = state.new_interval_variable(-100, 100, None);
        new_propagator(&mut state, a, b, c);

        state.propagate_to_fixed_point().expect("no empty domains");

        // Corners: 2*-4=-8, 2*-1=-2, 5*-4=-20, 5*-1=-5.
        state.assert_bounds(c, -20, -2);
    }

    #[test]
    fn both_straddling_zero_propagates_bounds_c() {
        let mut state = State::default();
        let a = state.new_interval_variable(-3, 4, None);
        let b = state.new_interval_variable(-2, 5, None);
        let c = state.new_interval_variable(-100, 100, None);
        new_propagator(&mut state, a, b, c);

        state.propagate_to_fixed_point().expect("no empty domains");

        // Corners: -3*-2=6, -3*5=-15, 4*-2=-8, 4*5=20.
        state.assert_bounds(c, -15, 20);
    }

    #[test]
    fn fixed_inconsistent_values_conflict() {
        let mut state = State::default();
        let a = state.new_interval_variable(3, 3, None);
        let b = state.new_interval_variable(4, 4, None);
        let c = state.new_interval_variable(11, 11, None);
        new_propagator(&mut state, a, b, c);

        let _ = state.propagate_to_fixed_point().unwrap_err();
    }

    #[test]
    fn checker_detects_a_pure_conflict_with_no_consequent() {
        // `consequent: None` is how the checker is invoked for a propagator-reported conflict
        // that isn't a single propagated predicate (see `VariableState::prepare_for_conflict_check`
        // and `State::check_conflict`). The checker must not just reject these outright: it needs
        // to confirm the premises alone are already contradictory.
        use pumpkin_checking::Comparison;
        use pumpkin_checking::TestAtomic;
        use pumpkin_checking::VariableState;

        let premises = [
            TestAtomic {
                name: "a",
                comparison: Comparison::Equal,
                value: 3,
            },
            TestAtomic {
                name: "b",
                comparison: Comparison::Equal,
                value: 4,
            },
            TestAtomic {
                name: "c",
                comparison: Comparison::Equal,
                value: 10,
            },
        ];

        let state = VariableState::prepare_for_conflict_check(premises, None)
            .expect("no conflicting atomics");

        let checker = IntegerMultiplicationChecker {
            a: "a",
            b: "b",
            c: "c",
        };

        // 3 * 4 = 12 != 10, so this is a genuine conflict.
        assert!(checker.check(state, &premises, None));
    }

    #[test]
    fn checker_does_not_report_a_conflict_for_consistent_premises_with_no_consequent() {
        use pumpkin_checking::Comparison;
        use pumpkin_checking::TestAtomic;
        use pumpkin_checking::VariableState;

        let premises = [TestAtomic {
            name: "a",
            comparison: Comparison::Equal,
            value: 3,
        }];

        let state = VariableState::prepare_for_conflict_check(premises, None)
            .expect("no conflicting atomics");

        let checker = IntegerMultiplicationChecker {
            a: "a",
            b: "b",
            c: "c",
        };

        // `b` and `c` are unconstrained, so `a = 3` alone can't be a conflict.
        assert!(!checker.check(state, &premises, None));
    }

    #[test]
    fn case1_denominator_and_numerator_straddle_zero_no_propagation() {
        let mut state = State::default();
        let a = state.new_interval_variable(-10, 10, None);
        let b = state.new_interval_variable(-2, 4, None);
        let c = state.new_interval_variable(-6, 6, None);
        new_propagator(&mut state, a, b, c);

        state.propagate_to_fixed_point().expect("no empty domains");

        state.assert_bounds(a, -10, 10);
    }

    #[test]
    fn case2_denominator_entirely_positive() {
        let mut state = State::default();
        // b = c / a, with `a` entirely positive.
        let a = state.new_interval_variable(2, 5, None);
        let b = state.new_interval_variable(-100, 100, None);
        let c = state.new_interval_variable(10, 20, None);
        new_propagator(&mut state, a, b, c);

        state.propagate_to_fixed_point().expect("no empty domains");

        // E2 = {10/2, 10/5, 20/2, 20/5} = {5, 2, 10, 4}.
        state.assert_bounds(b, 2, 10);

        // c's upper bound isn't needed: even with c unboundedly large, a <= 5 already caps how
        // small b = c/a can be forced to be relative to any larger c.
        let reason = reason_for(&mut state, predicate![b >= 2]);
        assert_eq!(conjunction!([c >= 10] & [a >= 2] & [a <= 5]), reason);
    }

    #[test]
    fn case2_denominator_entirely_negative() {
        let mut state = State::default();
        let a = state.new_interval_variable(-5, -2, None);
        let b = state.new_interval_variable(-100, 100, None);
        let c = state.new_interval_variable(10, 20, None);
        new_propagator(&mut state, a, b, c);

        state.propagate_to_fixed_point().expect("no empty domains");

        // E2 = {10/-5, 10/-2, 20/-5, 20/-2} = {-2, -5, -4, -10}.
        state.assert_bounds(b, -10, -2);
    }

    #[test]
    fn case3_split_denominator_straddles_zero() {
        let mut state = State::default();
        // a = c / b, with `b` straddling zero and `c` excluding zero.
        let a = state.new_interval_variable(-1000, 1000, None);
        let b = state.new_interval_variable(-2, 4, None);
        let c = state.new_interval_variable(6, 6, None);
        new_propagator(&mut state, a, b, c);

        state.propagate_to_fixed_point().expect("no empty domains");

        // Positive branch (b in [1,4]): E2 = {6/1, 6/4, 6/1, 6/4} -> [2, 6].
        // Negative branch (b in [-2,-1]): E2 = {6/-2, 6/-1, 6/-2, 6/-1} -> [-6, -3].
        // Combined hull: [-6, 6].
        state.assert_bounds(a, -6, 6);

        // `b` isn't needed at all: since c is fixed at 6, any a <= -7 would force |a * b| >= 7
        // for every nonzero integer b, and b = 0 gives a * b = 0 - so no integer b makes a * b =
        // 6 true, regardless of what b is allowed to be. Symmetrically for a >= 7.
        let reason = reason_for(&mut state, predicate![a >= -6]);
        assert_eq!(conjunction!([c >= 6] & [c <= 6]), reason);

        let reason = reason_for(&mut state, predicate![a <= 6]);
        assert_eq!(conjunction!([c >= 6] & [c <= 6]), reason);
    }

    #[test]
    fn case3_split_with_one_empty_half() {
        let mut state = State::default();
        // `b` touches zero only from the non-negative side, so the negative split half is empty.
        let a = state.new_interval_variable(-1000, 1000, None);
        let b = state.new_interval_variable(0, 4, None);
        let c = state.new_interval_variable(6, 6, None);
        new_propagator(&mut state, a, b, c);

        state.propagate_to_fixed_point().expect("no empty domains");

        // Only the positive branch (b in [1,4]) contributes: E2 = {6/1, 6/4} -> [2, 6].
        state.assert_bounds(a, 2, 6);
    }

    #[test]
    fn denominator_fixed_at_zero_leaves_other_variable_unconstrained() {
        let mut state = State::default();
        let a = state.new_interval_variable(0, 0, None);
        let b = state.new_interval_variable(-10, 10, None);
        let c = state.new_interval_variable(-5, 5, None);
        new_propagator(&mut state, a, b, c);

        state.propagate_to_fixed_point().expect("no empty domains");

        // a = 0 forces c = 0, but does not constrain b at all.
        state.assert_bounds(c, 0, 0);
        state.assert_bounds(b, -10, 10);
    }

    #[test]
    fn a_unbounded_does_not_panic() {
        let mut state = State::default();
        let a = state.new_interval_variable(i32::MIN, i32::MAX, None);
        let b = state.new_interval_variable(12, 12, None);
        let c = state.new_interval_variable(144, 144, None);
        new_propagator(&mut state, a, b, c);

        state.propagate_to_fixed_point().expect("No empty domains");
    }

    #[test]
    fn b_unbounded_does_not_panic() {
        let mut state = State::default();
        let a = state.new_interval_variable(12, 12, None);
        let b = state.new_interval_variable(i32::MIN, i32::MAX, None);
        let c = state.new_interval_variable(144, 144, None);
        new_propagator(&mut state, a, b, c);

        state.propagate_to_fixed_point().expect("No empty domains");
    }

    #[test]
    fn c_unbounded_does_not_panic() {
        let mut state = State::default();
        let a = state.new_interval_variable(12, 12, None);
        let b = state.new_interval_variable(12, 12, None);
        let c = state.new_interval_variable(i32::MIN, i32::MAX, None);
        new_propagator(&mut state, a, b, c);

        state.propagate_to_fixed_point().expect("No empty domains");
    }

    #[test]
    fn all_unbounded_does_not_panic() {
        let mut state = State::default();
        let a = state.new_interval_variable(i32::MIN, i32::MAX, None);
        let b = state.new_interval_variable(i32::MIN, i32::MAX, None);
        let c = state.new_interval_variable(i32::MIN, i32::MAX, None);
        new_propagator(&mut state, a, b, c);

        state.propagate_to_fixed_point().expect("No empty domains");
    }

    #[test]
    fn large_finite_domains_saturate_instead_of_overflowing() {
        let mut state = State::default();
        let a = state.new_interval_variable(1, 3, None);
        let b = state.new_interval_variable(1_000_000_000, 2_000_000_000, None);
        let c = state.new_interval_variable(i32::MIN, i32::MAX, None);
        new_propagator(&mut state, a, b, c);

        state.propagate_to_fixed_point().expect("No empty domains");

        // The true upper corner 3 * 2_000_000_000 = 6*10^9 is far beyond i32::MAX; it must
        // saturate rather than wrap or panic. The true lower corner 1 * 1_000_000_000 fits
        // comfortably and should not be affected by saturation.
        state.assert_bounds(c, 1_000_000_000, i32::MAX);
    }

    #[test]
    fn brute_force_cross_check_against_enumeration() {
        const RANGE: i32 = 5;

        for a_lo in -RANGE..=RANGE {
            for a_hi in a_lo..=RANGE {
                for b_lo in -RANGE..=RANGE {
                    for b_hi in b_lo..=RANGE {
                        let mut true_c_min = i32::MAX;
                        let mut true_c_max = i32::MIN;
                        let mut any_solution = false;

                        for av in a_lo..=a_hi {
                            for bv in b_lo..=b_hi {
                                let cv = av * bv;
                                if (-RANGE..=RANGE).contains(&cv) {
                                    any_solution = true;
                                    true_c_min = true_c_min.min(cv);
                                    true_c_max = true_c_max.max(cv);
                                }
                            }
                        }

                        if !any_solution {
                            continue;
                        }

                        let mut state = State::default();
                        let a = state.new_interval_variable(a_lo, a_hi, None);
                        let b = state.new_interval_variable(b_lo, b_hi, None);
                        let c = state.new_interval_variable(-RANGE, RANGE, None);
                        new_propagator(&mut state, a, b, c);

                        state
                            .propagate_to_fixed_point()
                            .expect("a real solution exists, so this must not conflict");

                        let propagated_c_min = state.lower_bound(c);
                        let propagated_c_max = state.upper_bound(c);

                        // The propagator must not exclude any true integer solution, and
                        // (being bounds(R)-consistent) must not be looser than the true
                        // bounds either.
                        assert!(
                            propagated_c_min <= true_c_min && propagated_c_max >= true_c_max,
                            "propagator excluded a valid solution for a in [{a_lo}..{a_hi}], \
                             b in [{b_lo}..{b_hi}]: propagated c in [{propagated_c_min}.\
                             .{propagated_c_max}], true c in [{true_c_min}..{true_c_max}]"
                        );
                    }
                }
            }
        }
    }
}
