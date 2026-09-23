use super::IntegerMultiplicationChecker;
use super::helpers::compute_quotient_bound_ext;
use super::helpers::product_bound_ext;
use crate::AtomicConstraint;
use crate::CheckerVariable;
use crate::InferenceChecker;
use crate::IntExt;
use crate::VariableState;

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
            Some(_) => unreachable!(),
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
