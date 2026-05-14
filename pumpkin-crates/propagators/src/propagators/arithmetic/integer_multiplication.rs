use std::cmp::max;
use std::cmp::min;

use pumpkin_checking::AtomicConstraint;
use pumpkin_checking::CheckerVariable;
use pumpkin_checking::InferenceChecker;
use pumpkin_core::conjunction;
use pumpkin_core::declare_inference_label;
use pumpkin_core::predicate;
use pumpkin_core::proof::ConstraintTag;
use pumpkin_core::proof::InferenceCode;
use pumpkin_core::propagation::DomainEvents;
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

    fn create(self, mut context: PropagatorConstructorContext) -> Self::PropagatorImpl {
        let IntegerMultiplicationArgs {
            a,
            b,
            c,
            constraint_tag,
        } = self;

        context.register(a.clone(), DomainEvents::ANY_INT, ID_A);
        context.register(b.clone(), DomainEvents::ANY_INT, ID_B);
        context.register(c.clone(), DomainEvents::ANY_INT, ID_C);

        IntegerMultiplicationPropagator {
            a,
            b,
            c,
            inference_code: InferenceCode::new(constraint_tag, IntegerMultiplication),
        }
    }
}

/// A bounds-consistent propagator for the constraint `a * b = c`.
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
    // First the propagation of the signs
    propagate_signs(&mut context, a, b, c, inference_code)?;

    let mut a_min = context.lower_bound(a);
    let mut a_max = context.upper_bound(a);
    let mut b_min = context.lower_bound(b);
    let mut b_max = context.upper_bound(b);

    // In achieving bounds consistency, the easier part is to propagate from a, b -> c
    // because c is bounded by all four corners using lower and upper bounds from a and b
    // c_ub <= max(a_lb * b_lb, a_lb * b_ub, a_ub * b_lb, a_ub * b_ub) and
    // c_lb >= min(a_lb * b_lb, a_lb * b_ub, a_ub * b_lb, a_ub * b_ub)
    context.post(
        predicate![
            c <= max(
                max(a_min.saturating_mul(b_min), a_min.saturating_mul(b_max)),
                max(a_max.saturating_mul(b_min), a_max.saturating_mul(b_max))
            )
        ],
        (
            conjunction!([a >= a_min] & [a <= a_max] & [b >= b_min] & [b <= b_max]),
            inference_code,
        ),
    )?;
    context.post(
        predicate![
            c >= min(
                min(a_min.saturating_mul(b_min), a_min.saturating_mul(b_max)),
                min(a_max.saturating_mul(b_min), a_max.saturating_mul(b_max))
            )
        ],
        (
            conjunction!([a >= a_min] & [a <= a_max] & [b >= b_min] & [b <= b_max]),
            inference_code,
        ),
    )?;

    // avoiding stale reads
    let mut c_min = context.lower_bound(c);
    let mut c_max = context.upper_bound(c);

    // For going c, b -> a division needs to be used which makes it less trivial.

    // If they both contain 0, skip propagation from c, b -> a because
    // it is impossible to bound `a` in this scenario because `0 * x = 0` for any `x`.
    if !((b_min <= 0 && b_max >= 0) && (c_min <= 0 && c_max >= 0)) {
        // need to check if b_min or b_max is exactly 0 to prevent division by zero.
        // this is possible because in this if block it is known c can't be 0.
        if b_min == 0 {
            context.post(
                predicate![b >= 1],
                (
                    conjunction!([b >= b_min] & [c >= c_min] & [c <= c_max]),
                    inference_code,
                ),
            )?;
        }
        if b_max == 0 {
            context.post(
                predicate![b <= -1],
                (
                    conjunction!([b <= b_max] & [c >= c_min] & [c <= c_max]),
                    inference_code,
                ),
            )?;
        }

        // need to re-read it to make sure to prevent division by 0
        b_min = context.lower_bound(b);
        b_max = context.upper_bound(b);

        // if b does not contain 0 it is trivial, just the same idea with some care with rounding
        if b_min > 0 || b_max < 0 {
            context.post(
                predicate![
                    a <= max(
                        max(floor_div(c_min, b_min), floor_div(c_min, b_max)),
                        max(floor_div(c_max, b_min), floor_div(c_max, b_max))
                    )
                ],
                (
                    conjunction!([c >= c_min] & [c <= c_max] & [b >= b_min] & [b <= b_max]),
                    inference_code,
                ),
            )?;
            context.post(
                predicate![
                    a >= min(
                        min(ceil_div(c_min, b_min), ceil_div(c_min, b_max)),
                        min(ceil_div(c_max, b_min), ceil_div(c_max, b_max))
                    )
                ],
                (
                    conjunction!([c >= c_min] & [c <= c_max] & [b >= b_min] & [b <= b_max]),
                    inference_code,
                ),
            )?;
        }
        // If b contains 0 but c does not, just using the four endpoint divisions can over-prune.
        // Values close to 0 (-1 or 1 in this case because this propagator uses integers) can
        // give the actual min/max, so they need to be considered separately.
        else {
            let initial_max = max(
                max(floor_div(c_min, b_min), floor_div(c_min, b_max)),
                max(floor_div(c_max, b_min), floor_div(c_max, b_max)),
            );
            let other_possible_max = max(
                max(c_min, c_min.saturating_mul(-1)),
                max(c_max, c_max.saturating_mul(-1)),
            );
            context.post(
                predicate![a <= max(initial_max, other_possible_max)],
                (
                    conjunction!([c >= c_min] & [c <= c_max] & [b >= b_min] & [b <= b_max]),
                    inference_code,
                ),
            )?;

            let initial_min = min(
                min(ceil_div(c_min, b_min), ceil_div(c_min, b_max)),
                min(ceil_div(c_max, b_min), ceil_div(c_max, b_max)),
            );
            let other_possible_min = min(
                min(c_min, c_min.saturating_mul(-1)),
                min(c_max, c_max.saturating_mul(-1)),
            );
            context.post(
                predicate![a >= min(initial_min, other_possible_min)],
                (
                    conjunction!([c >= c_min] & [c <= c_max] & [b >= b_min] & [b <= b_max]),
                    inference_code,
                ),
            )?;
        }
    }

    // avoiding stale read again
    a_min = context.lower_bound(a);
    a_max = context.upper_bound(a);

    // Now going from c, a -> b the exact same as c, b -> a
    // only difference is variable renaming
    if !((a_min <= 0 && a_max >= 0) && (c_min <= 0 && c_max >= 0)) {
        if a_min == 0 {
            context.post(
                predicate![a >= 1],
                (
                    conjunction!([a >= a_min] & [c >= c_min] & [c <= c_max]),
                    inference_code,
                ),
            )?;
        }
        if a_max == 0 {
            context.post(
                predicate![a <= -1],
                (
                    conjunction!([a <= a_max] & [c >= c_min] & [c <= c_max]),
                    inference_code,
                ),
            )?;
        }

        a_min = context.lower_bound(a);
        a_max = context.upper_bound(a);

        if a_min > 0 || a_max < 0 {
            context.post(
                predicate![
                    b <= max(
                        max(floor_div(c_min, a_min), floor_div(c_min, a_max)),
                        max(floor_div(c_max, a_min), floor_div(c_max, a_max))
                    )
                ],
                (
                    conjunction!([c >= c_min] & [c <= c_max] & [a >= a_min] & [a <= a_max]),
                    inference_code,
                ),
            )?;
            context.post(
                predicate![
                    b >= min(
                        min(ceil_div(c_min, a_min), ceil_div(c_min, a_max)),
                        min(ceil_div(c_max, a_min), ceil_div(c_max, a_max))
                    )
                ],
                (
                    conjunction!([c >= c_min] & [c <= c_max] & [a >= a_min] & [a <= a_max]),
                    inference_code,
                ),
            )?;
        } else {
            let initial_max = max(
                max(floor_div(c_min, a_min), floor_div(c_min, a_max)),
                max(floor_div(c_max, a_min), floor_div(c_max, a_max)),
            );
            let other_possible_max = max(
                max(c_min, c_min.saturating_mul(-1)),
                max(c_max, c_max.saturating_mul(-1)),
            );
            context.post(
                predicate![b <= max(initial_max, other_possible_max)],
                (
                    conjunction!([c >= c_min] & [c <= c_max] & [a >= a_min] & [a <= a_max]),
                    inference_code,
                ),
            )?;

            let initial_min = min(
                min(ceil_div(c_min, a_min), ceil_div(c_min, a_max)),
                min(ceil_div(c_max, a_min), ceil_div(c_max, a_max)),
            );
            let other_possible_min = min(
                min(c_min, c_min.saturating_mul(-1)),
                min(c_max, c_max.saturating_mul(-1)),
            );
            context.post(
                predicate![b >= min(initial_min, other_possible_min)],
                (
                    conjunction!([c >= c_min] & [c <= c_max] & [a >= a_min] & [a <= a_max]),
                    inference_code,
                ),
            )?;
        }
    }

    // b posted so read it to avoid stale read again
    b_min = context.lower_bound(b);
    b_max = context.upper_bound(b);

    // Bound points might not be in the result set, hence additional checks are needed.
    // The checks use a linear scan inward until a support is found, this can be costly in
    // some edge cases where the scan searches for a support for a big prime number.
    // This is a brute-force way of finding supports but it works cleanly for
    // most numbers because the initial pruning eliminates most of the possible values.

    // for lower bound of a
    while (a_min <= a_max) && (!has_support_for_fixed_factor(a_min, b_min, b_max, c_min, c_max)) {
        if a_min == i32::MAX {
            context.post(
                predicate![a != a_min],
                (
                    conjunction!(
                        [a == a_min] & [b >= b_min] & [b <= b_max] & [c >= c_min] & [c <= c_max]
                    ),
                    inference_code,
                ),
            )?;
        } else {
            context.post(
                predicate![a >= a_min.saturating_add(1)],
                (
                    conjunction!(
                        [a >= a_min] & [b >= b_min] & [b <= b_max] & [c >= c_min] & [c <= c_max]
                    ),
                    inference_code,
                ),
            )?;
        }

        a_min = context.lower_bound(a);
        a_max = context.upper_bound(a);
    }

    // for upper bound of a
    while (a_min <= a_max) && (!has_support_for_fixed_factor(a_max, b_min, b_max, c_min, c_max)) {
        if a_max == i32::MIN {
            context.post(
                predicate![a != a_max],
                (
                    conjunction!(
                        [a == a_max] & [b >= b_min] & [b <= b_max] & [c >= c_min] & [c <= c_max]
                    ),
                    inference_code,
                ),
            )?;
        } else {
            context.post(
                predicate![a <= a_max.saturating_sub(1)],
                (
                    conjunction!(
                        [a <= a_max] & [b >= b_min] & [b <= b_max] & [c >= c_min] & [c <= c_max]
                    ),
                    inference_code,
                ),
            )?;
        }

        a_min = context.lower_bound(a);
        a_max = context.upper_bound(a);
    }

    // for lower bound of b
    while (b_min <= b_max) && !has_support_for_fixed_factor(b_min, a_min, a_max, c_min, c_max) {
        if b_min == i32::MAX {
            context.post(
                predicate![b != b_min],
                (
                    conjunction!(
                        [b == b_min] & [a >= a_min] & [a <= a_max] & [c >= c_min] & [c <= c_max]
                    ),
                    inference_code,
                ),
            )?;
        } else {
            context.post(
                predicate![b >= b_min.saturating_add(1)],
                (
                    conjunction!(
                        [b >= b_min] & [a >= a_min] & [a <= a_max] & [c >= c_min] & [c <= c_max]
                    ),
                    inference_code,
                ),
            )?;
        }

        b_min = context.lower_bound(b);
        b_max = context.upper_bound(b);
    }

    // for upper bound of b
    while (b_min <= b_max) && !has_support_for_fixed_factor(b_max, a_min, a_max, c_min, c_max) {
        if b_max == i32::MIN {
            context.post(
                predicate![b != b_max],
                (
                    conjunction!(
                        [b == b_max] & [a >= a_min] & [a <= a_max] & [c >= c_min] & [c <= c_max]
                    ),
                    inference_code,
                ),
            )?;
        } else {
            context.post(
                predicate![b <= b_max.saturating_sub(1)],
                (
                    conjunction!(
                        [b <= b_max] & [a >= a_min] & [a <= a_max] & [c >= c_min] & [c <= c_max]
                    ),
                    inference_code,
                ),
            )?;
        }

        b_min = context.lower_bound(b);
        b_max = context.upper_bound(b);
    }

    // for lower bound of c
    while (c_min <= c_max) && (!has_support_for_c_bound(c_min, a_min, a_max, b_min, b_max)) {
        if c_min == i32::MAX {
            context.post(
                predicate![c != c_min],
                (
                    conjunction!(
                        [c == c_min] & [a >= a_min] & [a <= a_max] & [b >= b_min] & [b <= b_max]
                    ),
                    inference_code,
                ),
            )?;
        } else {
            context.post(
                predicate![c >= c_min.saturating_add(1)],
                (
                    conjunction!(
                        [c >= c_min] & [a >= a_min] & [a <= a_max] & [b >= b_min] & [b <= b_max]
                    ),
                    inference_code,
                ),
            )?;
        }

        c_min = context.lower_bound(c);
        c_max = context.upper_bound(c);
    }

    // for upper bound of c
    while (c_min <= c_max) && (!has_support_for_c_bound(c_max, a_min, a_max, b_min, b_max)) {
        if c_max == i32::MIN {
            context.post(
                predicate![c != c_max],
                (
                    conjunction!(
                        [c == c_max] & [a >= a_min] & [a <= a_max] & [b >= b_min] & [b <= b_max]
                    ),
                    inference_code,
                ),
            )?;
        } else {
            context.post(
                predicate![c <= c_max.saturating_sub(1)],
                (
                    conjunction!(
                        [c <= c_max] & [a >= a_min] & [a <= a_max] & [b >= b_min] & [b <= b_max]
                    ),
                    inference_code,
                ),
            )?;
        }

        c_min = context.lower_bound(c);
        c_max = context.upper_bound(c);
    }
    Ok(())
}

/// Computes ceiling of integer division `num / den`.
/// Handles `num == i32::MIN && den == -1` by returning `i32::MAX` to avoid overflow.
fn ceil_div(num: i32, den: i32) -> i32 {
    // Edge case check
    if num == i32::MIN && den == -1 {
        return i32::MAX;
    }

    let mut ans = num.saturating_div(den);
    let remainder = num % den;

    if remainder != 0 && ((num > 0 && den > 0) || (num < 0 && den < 0)) {
        ans = ans.saturating_add(1);
    }

    ans
}

/// Computes floor of integer division `num / den`.
/// Handles `num == i32::MIN && den == -1` by returning `i32::MAX` to avoid overflow.
fn floor_div(num: i32, den: i32) -> i32 {
    // Edge case check
    if num == i32::MIN && den == -1 {
        return i32::MAX;
    }

    let mut ans = num.saturating_div(den);
    let remainder = num % den;

    if remainder != 0 && ((num < 0 && den > 0) || (num > 0 && den < 0)) {
        ans = ans.saturating_sub(1);
    }

    ans
}

/// Checks if a fixed value of a or b can produce a product inside `[c_min, c_max]`
/// with some value in `[other_min, other_max]` to ensure bounds-consistency.
fn has_support_for_fixed_factor(
    fixed_value: i32,
    other_min: i32,
    other_max: i32,
    c_min: i32,
    c_max: i32,
) -> bool {
    if fixed_value == 0 {
        return c_min <= 0 && c_max >= 0;
    }

    let mut lower_bound_with_fixed_value = ceil_div(c_min, fixed_value);
    let mut upper_bound_with_fixed_value = floor_div(c_max, fixed_value);
    if fixed_value < 0 {
        lower_bound_with_fixed_value = ceil_div(c_max, fixed_value);
        upper_bound_with_fixed_value = floor_div(c_min, fixed_value);
    }

    // If this interval is empty, the fixed value cannot be supported.
    if lower_bound_with_fixed_value > upper_bound_with_fixed_value {
        return false;
    }

    if (other_min <= lower_bound_with_fixed_value && other_max >= lower_bound_with_fixed_value)
        || (other_max >= upper_bound_with_fixed_value && other_min <= upper_bound_with_fixed_value)
        || (other_min >= lower_bound_with_fixed_value && other_max <= upper_bound_with_fixed_value)
    {
        return true;
    }

    false
}

/// Checks if a candidate value of c can be achieved by some `a_val * b_val`
/// where `a_val` is in `[a_min, a_max]` and `b_val` is in `[b_min, b_max]`.
/// Scans the smaller of the two factor intervals to find a divisor.
/// This is a brute-force way of ensuring the support but it works sufficiently
/// because the initial pruning eliminates most possible values and the iteration
/// is made through the smaller interval.
fn has_support_for_c_bound(c_value: i32, a_min: i32, a_max: i32, b_min: i32, b_max: i32) -> bool {
    if c_value == 0 {
        // this can happen if at least one has 0 in their domain
        return (a_min <= 0 && a_max >= 0) || (b_min <= 0 && b_max >= 0);
    }

    // i64 is used here because it can overflow if the upper bound is
    // very large and the lower bound is very small
    let a_size = (a_max as i64) - (a_min as i64);
    let b_size = (b_max as i64) - (b_min as i64);

    if a_size <= b_size {
        let mut ctr = a_min;

        while ctr <= a_max {
            // Edge case check
            if ctr != 0 && !(c_value == i32::MIN && ctr == -1) && c_value % ctr == 0 {
                // no ceil or floor needed because it is an exact division here
                let is_this_in_the_set = c_value / ctr;
                if is_this_in_the_set >= b_min && is_this_in_the_set <= b_max {
                    return true;
                }
            }

            // Avoid infinite loops
            if ctr == i32::MAX {
                break;
            }

            ctr = ctr.saturating_add(1);
        }
    }
    // Else case is nearly the same, just variable changes
    else {
        let mut ctr = b_min;

        while ctr <= b_max {
            // Edge case check again
            if ctr != 0 && !(c_value == i32::MIN && ctr == -1) && c_value % ctr == 0 {
                // No ceil or floor needed because it is an exact division here
                let is_this_in_the_set = c_value / ctr;
                if is_this_in_the_set >= a_min && is_this_in_the_set <= a_max {
                    return true;
                }
            }

            // Avoid infinite loops
            if ctr == i32::MAX {
                break;
            }

            ctr = ctr.saturating_add(1);
        }
    }

    false
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
        state: pumpkin_checking::VariableState<Atomic>,
        _: &[Atomic],
        _: Option<&Atomic>,
    ) -> bool {
        // We apply interval arithmetic to determine that the computed interval `a times b`
        // does not intersect with the domain of `c`.
        //
        // See https://en.wikipedia.org/wiki/Interval_arithmetic#Interval_operators.

        let x1 = self.a.induced_lower_bound(&state);
        let x2 = self.a.induced_upper_bound(&state);
        let y1 = self.b.induced_lower_bound(&state);
        let y2 = self.b.induced_upper_bound(&state);

        let c_lower = self.c.induced_lower_bound(&state);
        let c_upper = self.c.induced_upper_bound(&state);

        let x1y1 = x1 * y1;
        let x1y2 = x1 * y2;
        let x2y1 = x2 * y1;
        let x2y2 = x2 * y2;

        let computed_c_lower = x1y1.min(x1y2).min(x2y1).min(x2y2);
        let computed_c_upper = x1y1.max(x1y2).max(x2y1).max(x2y2);

        if computed_c_upper < c_lower || computed_c_lower > c_upper {
            return true;
        }

        // The corner check above can miss a conflict when one of the factors crosses 0.
        // When that happens the interval arithmetic can become -inf to +inf which always
        // intersects with c even if the negative and positive parts are both impossible.
        // To prevent that, it splits the crossed boundary to negative, positive and the 0 part
        let zero = pumpkin_checking::IntExt::Int(0);
        let neg_one = pumpkin_checking::IntExt::Int(-1);
        let one = pumpkin_checking::IntExt::Int(1);

        // Checks if the interval [lo, hi] is completely outside c's current interval.
        let intervals_disjoint = |lo: pumpkin_checking::IntExt,
                                  hi: pumpkin_checking::IntExt,
                                  c_lo: pumpkin_checking::IntExt,
                                  c_hi: pumpkin_checking::IntExt|
         -> bool { hi < c_lo || lo > c_hi };

        // First try the case where b crosses 0.
        if y1 < zero && y2 > zero {
            // If c can still be 0 then b = 0 is a possible support, so this split
            // is not enough to prove a conflict.
            let zero_is_supported = c_lower <= zero && c_upper >= zero;
            if !zero_is_supported {
                // Check b in [blow, -1]
                let corners_neg = [x1 * y1, x1 * neg_one, x2 * y1, x2 * neg_one];
                let neg_lo = corners_neg.iter().copied().min().unwrap();
                let neg_hi = corners_neg.iter().copied().max().unwrap();

                // Check b in [1, bhigh]
                let corners_pos = [x1 * one, x1 * y2, x2 * one, x2 * y2];
                let pos_lo = corners_pos.iter().copied().min().unwrap();
                let pos_hi = corners_pos.iter().copied().max().unwrap();

                // If neither side can overlap c, and b = 0 was already impossible,
                // then that means there is a conflict.
                if intervals_disjoint(neg_lo, neg_hi, c_lower, c_upper)
                    && intervals_disjoint(pos_lo, pos_hi, c_lower, c_upper)
                {
                    return true;
                }
            }
        }

        // Same thing, but now for the case where a crosses 0.
        if x1 < zero && x2 > zero {
            let zero_is_supported = c_lower <= zero && c_upper >= zero;
            if !zero_is_supported {
                let corners_neg = [x1 * y1, x1 * y2, neg_one * y1, neg_one * y2];
                let neg_lo = corners_neg.iter().copied().min().unwrap();
                let neg_hi = corners_neg.iter().copied().max().unwrap();

                let corners_pos = [one * y1, one * y2, x2 * y1, x2 * y2];
                let pos_lo = corners_pos.iter().copied().min().unwrap();
                let pos_hi = corners_pos.iter().copied().max().unwrap();

                if intervals_disjoint(neg_lo, neg_hi, c_lower, c_upper)
                    && intervals_disjoint(pos_lo, pos_hi, c_lower, c_upper)
                {
                    return true;
                }
            }
        }

        // If a bound is not mentioned in the reason, the checker sees it as
        // unbounded. Solver integer variables are i32-bounded, so map those
        // infinities to the full i32 range before using the support helpers.
        let to_i32 = |v: pumpkin_checking::IntExt| -> i32 {
            match v {
                pumpkin_checking::IntExt::Int(i) => i,
                pumpkin_checking::IntExt::NegativeInf => i32::MIN,
                pumpkin_checking::IntExt::PositiveInf => i32::MAX,
            }
        };
        let x1_conv = to_i32(x1);
        let x2_conv = to_i32(x2);
        let y1_conv = to_i32(y1);
        let y2_conv = to_i32(y2);
        let c_lower_conv = to_i32(c_lower);
        let c_upper_conv = to_i32(c_upper);

        // Also check that fixed bounds actually have support.
        if x1_conv == x2_conv
            && !has_support_for_fixed_factor(x1_conv, y1_conv, y2_conv, c_lower_conv, c_upper_conv)
        {
            return true;
        }

        if y1_conv == y2_conv
            && !has_support_for_fixed_factor(y1_conv, x1_conv, x2_conv, c_lower_conv, c_upper_conv)
        {
            return true;
        }

        if c_lower_conv == c_upper_conv
            && !has_support_for_c_bound(c_lower_conv, x1_conv, x2_conv, y1_conv, y2_conv)
        {
            return true;
        }

        false
    }
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

    #[test]
    fn bounds_of_a_and_b_propagate_bounds_c() {
        let mut state = State::default();
        let a = state.new_interval_variable(1, 3, None);
        let b = state.new_interval_variable(0, 4, None);
        let c = state.new_interval_variable(-10, 20, None);

        let constraint_tag = state.new_constraint_tag();

        let _ = state.add_propagator(IntegerMultiplicationArgs {
            a,
            b,
            c,
            constraint_tag,
        });
        state.propagate_to_fixed_point().expect("no empty domains");

        state.assert_bounds(a, 1, 3);
        state.assert_bounds(b, 0, 4);
        state.assert_bounds(c, 0, 12);

        let mut reason_buffer: Vec<Predicate> = vec![];
        let _ = state.get_propagation_reason(
            predicate![c >= 0],
            &mut reason_buffer,
            CurrentNogood::empty(),
        );
        let reason_lb: PropositionalConjunction = reason_buffer.into();
        assert_eq!(conjunction!([a >= 0] & [b >= 0]), reason_lb);

        let mut reason_buffer: Vec<Predicate> = vec![];
        let _ = state.get_propagation_reason(
            predicate![c <= 12],
            &mut reason_buffer,
            CurrentNogood::empty(),
        );
        let reason_ub: PropositionalConjunction = reason_buffer.into();
        assert_eq!(
            conjunction!([a >= 1] & [a <= 3] & [b >= 0] & [b <= 4]),
            reason_ub
        );
    }

    #[test]
    fn bounds_of_a_and_c_propagate_bounds_b() {
        let mut state = State::default();
        let a = state.new_interval_variable(2, 3, None);
        let b = state.new_interval_variable(0, 12, None);
        let c = state.new_interval_variable(2, 12, None);

        let constraint_tag = state.new_constraint_tag();

        let _ = state.add_propagator(IntegerMultiplicationArgs {
            a,
            b,
            c,
            constraint_tag,
        });
        state.propagate_to_fixed_point().expect("no empty domains");

        state.assert_bounds(a, 2, 3);
        state.assert_bounds(b, 1, 6);
        state.assert_bounds(c, 2, 12);

        let mut reason_buffer: Vec<Predicate> = vec![];
        let _ = state.get_propagation_reason(
            predicate![b >= 1],
            &mut reason_buffer,
            CurrentNogood::empty(),
        );
        let reason_lb: PropositionalConjunction = reason_buffer.into();
        assert_eq!(conjunction!([a >= 1] & [c >= 1]), reason_lb);

        let mut reason_buffer: Vec<Predicate> = vec![];
        let _ = state.get_propagation_reason(
            predicate![b <= 6],
            &mut reason_buffer,
            CurrentNogood::empty(),
        );
        let reason_ub: PropositionalConjunction = reason_buffer.into();
        assert_eq!(
            conjunction!([a >= 2] & [a <= 3] & [c >= 2] & [c <= 12]),
            reason_ub
        );
    }

    #[test]
    fn bounds_of_b_and_c_propagate_bounds_a() {
        let mut state = State::default();
        let a = state.new_interval_variable(0, 10, None);
        let b = state.new_interval_variable(3, 6, None);
        let c = state.new_interval_variable(2, 12, None);

        let constraint_tag = state.new_constraint_tag();

        let _ = state.add_propagator(IntegerMultiplicationArgs {
            a,
            b,
            c,
            constraint_tag,
        });
        state.propagate_to_fixed_point().expect("no empty domains");

        state.assert_bounds(a, 1, 4);
        state.assert_bounds(b, 3, 6);
        state.assert_bounds(c, 3, 12);

        let mut reason_buffer: Vec<Predicate> = vec![];
        let _ = state.get_propagation_reason(
            predicate![a >= 1],
            &mut reason_buffer,
            CurrentNogood::empty(),
        );
        let reason_lb: PropositionalConjunction = reason_buffer.into();
        assert_eq!(conjunction!([b >= 1] & [c >= 1]), reason_lb);

        let mut reason_buffer: Vec<Predicate> = vec![];
        let _ = state.get_propagation_reason(
            predicate![a <= 4],
            &mut reason_buffer,
            CurrentNogood::empty(),
        );
        let reason_ub: PropositionalConjunction = reason_buffer.into();
        assert_eq!(
            conjunction!([b >= 3] & [b <= 6] & [c >= 3] & [c <= 12]),
            reason_ub
        );
    }

    #[test]
    fn bounds_consistency_prunes_unsupported_c_lower_bound_prime() {
        let mut state = State::default();
        let a = state.new_interval_variable(2, 10, None);
        let b = state.new_interval_variable(2, 10, None);
        let c = state.new_interval_variable(5, 100, None);

        let constraint_tag = state.new_constraint_tag();

        let _ = state.add_propagator(IntegerMultiplicationArgs {
            a,
            b,
            c,
            constraint_tag,
        });

        state.propagate_to_fixed_point().expect("no empty domains");

        state.assert_bounds(c, 6, 100);
    }

    #[test]
    fn bounds_consistency_prunes_unsupported_c_upper_bound() {
        let mut state = State::default();
        let a = state.new_interval_variable(2, 10, None);
        let b = state.new_interval_variable(2, 10, None);
        let c = state.new_interval_variable(4, 99, None);

        let constraint_tag = state.new_constraint_tag();

        let _ = state.add_propagator(IntegerMultiplicationArgs {
            a,
            b,
            c,
            constraint_tag,
        });

        state.propagate_to_fixed_point().expect("no empty domains");

        state.assert_bounds(c, 4, 90);
    }

    #[test]
    fn bounds_consistency_detects_fixed_unsupported_product() {
        let mut state = State::default();
        let a = state.new_interval_variable(2, 3, None);
        let b = state.new_interval_variable(2, 3, None);
        let c = state.new_interval_variable(5, 5, None);

        let constraint_tag = state.new_constraint_tag();

        let _ = state.add_propagator(IntegerMultiplicationArgs {
            a,
            b,
            c,
            constraint_tag,
        });

        assert!(state.propagate_to_fixed_point().is_err());
    }

    #[test]
    fn bounds_consistency_prunes_unsupported_a_lower_bound() {
        let mut state = State::default();
        let a = state.new_interval_variable(2, 10, None);
        let b = state.new_interval_variable(4, 4, None);
        let c = state.new_interval_variable(20, 40, None);

        let constraint_tag = state.new_constraint_tag();

        let _ = state.add_propagator(IntegerMultiplicationArgs {
            a,
            b,
            c,
            constraint_tag,
        });

        state.propagate_to_fixed_point().expect("no empty domains");

        state.assert_bounds(a, 5, 10);
    }

    #[test]
    fn bounds_consistency_prunes_unsupported_b_upper_bound() {
        let mut state = State::default();
        let a = state.new_interval_variable(5, 5, None);
        let b = state.new_interval_variable(1, 10, None);
        let c = state.new_interval_variable(12, 27, None);

        let constraint_tag = state.new_constraint_tag();

        let _ = state.add_propagator(IntegerMultiplicationArgs {
            a,
            b,
            c,
            constraint_tag,
        });

        state.propagate_to_fixed_point().expect("no empty domains");

        state.assert_bounds(b, 3, 5);
    }

    #[test]
    fn bounds_consistency_handles_negative_supported_c_bound() {
        let mut state = State::default();
        let a = state.new_interval_variable(-5, -2, None);
        let b = state.new_interval_variable(2, 5, None);
        let c = state.new_interval_variable(-11, -4, None);

        let constraint_tag = state.new_constraint_tag();

        let _ = state.add_propagator(IntegerMultiplicationArgs {
            a,
            b,
            c,
            constraint_tag,
        });

        state.propagate_to_fixed_point().expect("no empty domains");

        state.assert_bounds(c, -10, -4);
    }

    #[test]
    fn b_unbounded_does_not_panic() {
        let mut state = State::default();
        let a = state.new_interval_variable(12, 12, None);
        let b = state.new_interval_variable(i32::MIN, i32::MAX, None);
        let c = state.new_interval_variable(144, 144, None);

        let constraint_tag = state.new_constraint_tag();
        let _ = state.add_propagator(IntegerMultiplicationArgs {
            a,
            b,
            c,
            constraint_tag,
        });
        state.propagate_to_fixed_point().expect("No empty domains");
    }

    #[test]
    fn a_unbounded_does_not_panic() {
        let mut state = State::default();
        let a = state.new_interval_variable(i32::MIN, i32::MAX, None);
        let b = state.new_interval_variable(12, 12, None);
        let c = state.new_interval_variable(144, 144, None);

        let constraint_tag = state.new_constraint_tag();
        let _ = state.add_propagator(IntegerMultiplicationArgs {
            a,
            b,
            c,
            constraint_tag,
        });
        state.propagate_to_fixed_point().expect("No empty domains");
    }

    #[test]
    fn c_unbounded_does_not_panic() {
        let mut state = State::default();
        let a = state.new_interval_variable(12, 12, None);
        let b = state.new_interval_variable(12, 12, None);
        let c = state.new_interval_variable(i32::MIN, i32::MAX, None);

        let constraint_tag = state.new_constraint_tag();
        let _ = state.add_propagator(IntegerMultiplicationArgs {
            a,
            b,
            c,
            constraint_tag,
        });
        state.propagate_to_fixed_point().expect("No empty domains");
    }

    #[test]
    fn all_unbounded_does_not_panic() {
        let mut state = State::default();
        let a = state.new_interval_variable(i32::MIN, i32::MAX, None);
        let b = state.new_interval_variable(i32::MIN, i32::MAX, None);
        let c = state.new_interval_variable(i32::MIN, i32::MAX, None);

        let constraint_tag = state.new_constraint_tag();
        let _ = state.add_propagator(IntegerMultiplicationArgs {
            a,
            b,
            c,
            constraint_tag,
        });
        state.propagate_to_fixed_point().expect("No empty domains");
    }
}
