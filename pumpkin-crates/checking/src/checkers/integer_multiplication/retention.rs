use super::IntegerMultiplicationChecker;
use super::helpers::compute_quotient_bound_ext;
use super::helpers::product_bound_ext;
use crate::AtomicConstraint;
use crate::CheckerVariable;
use crate::IntExt;
use crate::RetentionCheck;
use crate::RetentionChecker;
use crate::VariableState;

/// Mirrors one pass of the propagation of the multiplication propagator: the propagator has
/// nothing left to propagate if `c` lies within the products of the bounds of `a` and `b`, and `a`
/// and `b` lie within the quotients of the bounds of `c` and the other factor.
impl<VA, VB, VC, Atomic> RetentionChecker<Atomic> for IntegerMultiplicationChecker<VA, VB, VC>
where
    Atomic: AtomicConstraint,
    VA: CheckerVariable<Atomic>,
    VB: CheckerVariable<Atomic>,
    VC: CheckerVariable<Atomic>,
{
    fn check_retention(&self, state: &VariableState<Atomic>) -> RetentionCheck {
        let (a_min, a_max) = bounds(&self.a, state);
        let (b_min, b_max) = bounds(&self.b, state);
        let (c_min, c_max) = bounds(&self.c, state);

        // c = a * b
        let (c_lo, c_hi) = product_bound_ext(a_min, a_max, b_min, b_max);
        if c_min < c_lo || c_max > c_hi {
            log::error!(
                "The bounds of {:?} could be tightened to [{c_lo:?}, {c_hi:?}] by the product of {:?} and {:?}",
                self.c,
                self.a,
                self.b
            );
            return RetentionCheck::PropagationMissed;
        }

        // a = c / b
        if let Some((lo, hi)) = tightened_quotient((c_min, c_max), (b_min, b_max), (a_min, a_max)) {
            log::error!(
                "The bounds of {:?} could be tightened to [{lo:?}, {hi:?}] by {:?} / {:?}",
                self.a,
                self.c,
                self.b
            );
            return RetentionCheck::PropagationMissed;
        }

        // b = c / a
        if let Some((lo, hi)) = tightened_quotient((c_min, c_max), (a_min, a_max), (b_min, b_max)) {
            log::error!(
                "The bounds of {:?} could be tightened to [{lo:?}, {hi:?}] by {:?} / {:?}",
                self.b,
                self.c,
                self.a
            );
            return RetentionCheck::PropagationMissed;
        }

        RetentionCheck::NothingToPropagate
    }
}

/// The bounds of `target` in `target * denominator = numerator`, if they are tighter than
/// `target`.
fn tightened_quotient(
    (numerator_min, numerator_max): (IntExt<i64>, IntExt<i64>),
    (denominator_min, denominator_max): (IntExt<i64>, IntExt<i64>),
    (target_min, target_max): (IntExt<i64>, IntExt<i64>),
) -> Option<(IntExt<i64>, IntExt<i64>)> {
    let (lo, hi) = compute_quotient_bound_ext(
        numerator_min,
        numerator_max,
        denominator_min,
        denominator_max,
    )?;
    (target_min < lo || target_max > hi).then_some((lo, hi))
}

fn bounds<Atomic: AtomicConstraint, Var: CheckerVariable<Atomic>>(
    variable: &Var,
    state: &VariableState<Atomic>,
) -> (IntExt<i64>, IntExt<i64>) {
    (
        variable.induced_lower_bound(state).into(),
        variable.induced_upper_bound(state).into(),
    )
}
