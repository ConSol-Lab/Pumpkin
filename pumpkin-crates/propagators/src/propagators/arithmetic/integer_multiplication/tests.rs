use pumpkin_core::conjunction;
use pumpkin_core::predicate;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::predicates::PropositionalConjunction;
use pumpkin_core::propagation::CurrentNogood;
use pumpkin_core::state::State;
use pumpkin_core::variables::TransformableVariable;

use super::super::IntegerMultiplicationConstructor;
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
    let _ = state.add_propagator(IntegerMultiplicationConstructor {
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
    // itself: the explainer must not assume the two coincide.
    let mut state = State::default();
    let a = state.new_interval_variable(1, 3, None);
    let b = state.new_interval_variable(0, 4, None);
    let c_underlying = state.new_interval_variable(-20, 10, None);
    let c = c_underlying.scaled(-1);

    let constraint_tag = state.new_constraint_tag();
    let _ = state.add_propagator(IntegerMultiplicationConstructor {
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
