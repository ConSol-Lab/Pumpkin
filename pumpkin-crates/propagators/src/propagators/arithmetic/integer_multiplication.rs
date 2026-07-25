use pumpkin_checking::AtomicConstraint;
use pumpkin_checking::CheckerVariable;
use pumpkin_checking::InferenceChecker;
use pumpkin_checking::IntExt;
use pumpkin_checking::VariableState;
use pumpkin_core::conjunction;
use pumpkin_core::declare_inference_label;
use pumpkin_core::predicate;
use pumpkin_core::proof::ConstraintTag;
use pumpkin_core::proof::InferenceCode;
use pumpkin_core::propagation::DomainEvents;
use pumpkin_core::propagation::EventsToRegister;
use pumpkin_core::propagation::InferenceCheckers;
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
        };

        (registration, propagator)
    }
}

/// A propagator for maintaining the constraint `a * b = c`.
///
/// The propagator is bounds(R)-consistent, following Schulte & Stuckey, "When Do Bounds and
/// Domain Propagation Lead to the Same Search Space?" (ACM TOPLAS 27(3), 2005), §2.3.
#[derive(Clone, Debug)]
pub struct IntegerMultiplicationPropagator<VA, VB, VC> {
    a: VA,
    b: VB,
    c: VC,
    inference_code: InferenceCode,
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
    let a_min = context.lower_bound(a);
    let a_max = context.upper_bound(a);
    let b_min = context.lower_bound(b);
    let b_max = context.upper_bound(b);
    let c_min = context.lower_bound(c);
    let c_max = context.upper_bound(c);

    // c = a * b
    let (c_lo, c_hi) = product_bound(a_min as i64, a_max as i64, b_min as i64, b_max as i64);
    let ab_reason = conjunction!([a >= a_min] & [a <= a_max] & [b >= b_min] & [b <= b_max]);
    context.post(
        predicate![c >= saturate_i64_to_i32(c_lo)],
        (ab_reason.clone(), inference_code),
    )?;
    context.post(
        predicate![c <= saturate_i64_to_i32(c_hi)],
        (ab_reason, inference_code),
    )?;

    // a = c / b
    propagate_quotient(
        &mut context,
        Operand {
            var: c,
            min: c_min,
            max: c_max,
        },
        Operand {
            var: b,
            min: b_min,
            max: b_max,
        },
        a,
        inference_code,
    )?;

    // b = c / a
    propagate_quotient(
        &mut context,
        Operand {
            var: c,
            min: c_min,
            max: c_max,
        },
        Operand {
            var: a,
            min: a_min,
            max: a_max,
        },
        b,
        inference_code,
    )?;

    Ok(())
}

/// One of the two operand variables in `target * denominator = numerator`, together with the
/// domain bounds it was snapshotted at.
struct Operand<'a, V> {
    var: &'a V,
    min: i32,
    max: i32,
}

/// Propagates the bounds of `target` in `target * denominator = numerator`.
fn propagate_quotient<VNum: IntegerVariable, VDen: IntegerVariable, VTarget: IntegerVariable>(
    context: &mut PropagationContext,
    numerator: Operand<VNum>,
    denominator: Operand<VDen>,
    target: &VTarget,
    inference_code: &InferenceCode,
) -> PropagationStatusCP {
    let Some((lo, hi)) = compute_quotient_bound(
        numerator.min as i64,
        numerator.max as i64,
        denominator.min as i64,
        denominator.max as i64,
    ) else {
        return Ok(());
    };

    let reason = conjunction!(
        [numerator.var >= numerator.min]
            & [numerator.var <= numerator.max]
            & [denominator.var >= denominator.min]
            & [denominator.var <= denominator.max]
    );

    context.post(
        predicate![target >= saturate_i64_to_i32(lo)],
        (reason.clone(), inference_code),
    )?;
    context.post(
        predicate![target <= saturate_i64_to_i32(hi)],
        (reason, inference_code),
    )?;

    Ok(())
}

/// Computes `[min E1 .. max E1]` where `E1` is the set of the four corner products of `[a_min ..
/// a_max] x [b_min .. b_max]`.
fn product_bound(a_min: i64, a_max: i64, b_min: i64, b_max: i64) -> (i64, i64) {
    // Each factor is within `i32` range, so the product comfortably fits in `i64` without
    // overflowing.
    let corners = [a_min * b_min, a_min * b_max, a_max * b_min, a_max * b_max];

    (
        corners.into_iter().min().expect("corners is non-empty"),
        corners.into_iter().max().expect("corners is non-empty"),
    )
}

/// Computes `[ceil(inf E2) .. floor(sup E2)]` where `E2` is the set of the four corner quotients
/// of `[num_min .. num_max] / [den_min .. den_max]`.
///
/// Assumes `[den_min .. den_max]` does not contain zero.
fn quotient_bound(num_min: i64, num_max: i64, den_min: i64, den_max: i64) -> (i64, i64) {
    assert!(den_min > 0 || den_max < 0);

    let lo = [num_min, num_max]
        .into_iter()
        .flat_map(|n| {
            [den_min, den_max]
                .into_iter()
                .map(move |d| div_ceil_i64(n, d))
        })
        .min()
        .expect("corners is non-empty");
    let hi = [num_min, num_max]
        .into_iter()
        .flat_map(|n| {
            [den_min, den_max]
                .into_iter()
                .map(move |d| div_floor_i64(n, d))
        })
        .max()
        .expect("corners is non-empty");

    (lo, hi)
}

/// Computes the tightest range for `target` in `target * denominator = numerator`, or `None` if
/// no propagation is possible.
fn compute_quotient_bound(
    num_min: i64,
    num_max: i64,
    den_min: i64,
    den_max: i64,
) -> Option<(i64, i64)> {
    let den_straddles_zero = den_min <= 0 && den_max >= 0;
    let num_straddles_zero = num_min <= 0 && num_max >= 0;

    if den_straddles_zero && num_straddles_zero {
        // Both the numerator and the denominator could be zero, so no value of `target` can be
        // ruled out.
        return None;
    }

    if !den_straddles_zero {
        return Some(quotient_bound(num_min, num_max, den_min, den_max));
    }

    // `denominator` contains zero but `numerator` does not, so `denominator` cannot actually be
    // zero (that would force `numerator` to be zero too). Split `denominator`'s domain at zero
    // and combine the bound derived from each half.
    let branch_pos = (den_max >= 1).then(|| quotient_bound(num_min, num_max, 1, den_max));
    let branch_neg = (den_min <= -1).then(|| quotient_bound(num_min, num_max, den_min, -1));

    match (branch_pos, branch_neg) {
        (Some((lo_pos, hi_pos)), Some((lo_neg, hi_neg))) => {
            Some((lo_pos.min(lo_neg), hi_pos.max(hi_neg)))
        }
        (Some(bound), None) | (None, Some(bound)) => Some(bound),
        // Unreachable in practice: this means `denominator` is fixed to exactly zero, which
        // would already have forced `numerator`'s bound to include zero (and hence conflicted,
        // since `numerator` here excludes it) via the `c = a * b` propagation computed earlier
        // from the same snapshot.
        (None, None) => None,
    }
}

/// Division with rounding up. Assumes `denominator != 0` and that neither operand is close
/// enough to `i64::MIN`/`i64::MAX` to overflow (guaranteed here since both are derived from
/// `i32` values).
fn div_ceil_i64(numerator: i64, denominator: i64) -> i64 {
    let d = numerator / denominator;
    let r = numerator % denominator;
    if (r > 0 && denominator > 0) || (r < 0 && denominator < 0) {
        d + 1
    } else {
        d
    }
}

/// Division with rounding down. Assumes `denominator != 0` and that neither operand is close
/// enough to `i64::MIN`/`i64::MAX` to overflow (guaranteed here since both are derived from
/// `i32` values).
fn div_floor_i64(numerator: i64, denominator: i64) -> i64 {
    let d = numerator / denominator;
    let r = numerator % denominator;
    if (r > 0 && denominator < 0) || (r < 0 && denominator > 0) {
        d - 1
    } else {
        d
    }
}

fn saturate_i64_to_i32(value: i64) -> i32 {
    value.clamp(i32::MIN as i64, i32::MAX as i64) as i32
}

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
        // Independently recompute the same bounds(R)-consistent bound the propagator would
        // derive, using only the cited premises (via `induced_*_bound`), and confirm it excludes
        // the negated consequent. The propagator's reasons always cite the *full* range of both
        // operand variables (never a single corner-specific bound, and never the target's own
        // bound), so the two operands' induced bounds are always finite here.
        let Some(atomic) = consequent else {
            return false;
        };

        if self.c.does_atomic_constrain_self(atomic) {
            let a_min = expect_finite(self.a.induced_lower_bound(&state));
            let a_max = expect_finite(self.a.induced_upper_bound(&state));
            let b_min = expect_finite(self.b.induced_lower_bound(&state));
            let b_max = expect_finite(self.b.induced_upper_bound(&state));

            let (lo, hi) = product_bound(a_min, a_max, b_min, b_max);
            is_disjoint(
                lo,
                hi,
                self.c.induced_lower_bound(&state),
                self.c.induced_upper_bound(&state),
            )
        } else if self.a.does_atomic_constrain_self(atomic) {
            let b_min = expect_finite(self.b.induced_lower_bound(&state));
            let b_max = expect_finite(self.b.induced_upper_bound(&state));
            let c_min = expect_finite(self.c.induced_lower_bound(&state));
            let c_max = expect_finite(self.c.induced_upper_bound(&state));

            let Some((lo, hi)) = compute_quotient_bound(c_min, c_max, b_min, b_max) else {
                return false;
            };
            is_disjoint(
                lo,
                hi,
                self.a.induced_lower_bound(&state),
                self.a.induced_upper_bound(&state),
            )
        } else if self.b.does_atomic_constrain_self(atomic) {
            let a_min = expect_finite(self.a.induced_lower_bound(&state));
            let a_max = expect_finite(self.a.induced_upper_bound(&state));
            let c_min = expect_finite(self.c.induced_lower_bound(&state));
            let c_max = expect_finite(self.c.induced_upper_bound(&state));

            let Some((lo, hi)) = compute_quotient_bound(c_min, c_max, a_min, a_max) else {
                return false;
            };
            is_disjoint(
                lo,
                hi,
                self.b.induced_lower_bound(&state),
                self.b.induced_upper_bound(&state),
            )
        } else {
            false
        }
    }
}

fn expect_finite(bound: IntExt) -> i64 {
    bound
        .as_int()
        .expect(
            "the multiplication propagator's reasons always cite the full range of the two \
             operand variables",
        )
        .into()
}

fn is_disjoint(lo: i64, hi: i64, induced_lo: IntExt, induced_hi: IntExt) -> bool {
    induced_hi < saturate_i64_to_i32(lo) || induced_lo > saturate_i64_to_i32(hi)
}

#[cfg(test)]
mod tests {
    use pumpkin_core::predicate;
    use pumpkin_core::predicates::Predicate;
    use pumpkin_core::predicates::PropositionalConjunction;
    use pumpkin_core::propagation::CurrentNogood;
    use pumpkin_core::state::State;

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

        let reason = reason_for(&mut state, predicate![c >= 0]);
        assert_eq!(
            conjunction!([a >= 1] & [a <= 3] & [b >= 0] & [b <= 4]),
            reason
        );

        let reason = reason_for(&mut state, predicate![c <= 12]);
        assert_eq!(
            conjunction!([a >= 1] & [a <= 3] & [b >= 0] & [b <= 4]),
            reason
        );
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

        let reason = reason_for(&mut state, predicate![b >= 2]);
        assert_eq!(
            conjunction!([c >= 10] & [c <= 20] & [a >= 2] & [a <= 5]),
            reason
        );
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

        let reason = reason_for(&mut state, predicate![a >= -6]);
        assert_eq!(
            conjunction!([b >= -2] & [b <= 4] & [c >= 6] & [c <= 6]),
            reason
        );

        let reason = reason_for(&mut state, predicate![a <= 6]);
        assert_eq!(
            conjunction!([b >= -2] & [b <= 4] & [c >= 6] & [c <= 6]),
            reason
        );
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
