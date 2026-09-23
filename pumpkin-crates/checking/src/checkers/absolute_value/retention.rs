use super::AbsoluteValueChecker;
use crate::AtomicConstraint;
use crate::CheckerVariable;
use crate::RetentionChecker;
use crate::VariableState;
use crate::checkers::retention_checker::scope_lower_bound;
use crate::checkers::retention_checker::scope_upper_bound;

impl<VA, VB, Atomic> RetentionChecker<Atomic> for AbsoluteValueChecker<VA, VB>
where
    VA: CheckerVariable<Atomic>,
    VB: CheckerVariable<Atomic>,
    Atomic: AtomicConstraint,
{
    fn check_retention(&self, state: &VariableState<Atomic>) -> bool {
        let signed_lower = i64::from(scope_lower_bound(&self.signed, state));
        let signed_upper = i64::from(scope_upper_bound(&self.signed, state));
        let absolute_lower = i64::from(scope_lower_bound(&self.absolute, state));
        let absolute_upper = i64::from(scope_upper_bound(&self.absolute, state));

        let greatest_absolute = signed_lower.abs().max(signed_upper.abs());
        let least_absolute = if signed_lower <= 0 && 0 <= signed_upper {
            0
        } else {
            signed_lower.abs().min(signed_upper.abs())
        };

        // 1. Assert that the lower bound of absolute is at least the least absolute value of
        //    signed, which is 0 when signed can be 0
        if absolute_lower < least_absolute {
            log::error!(
                "The lower bound of {:?} could be raised to {least_absolute} by the absolute value of {:?}",
                self.absolute,
                self.signed
            );
            return false;
        }

        // 2. Assert that the upper bound of absolute equals the greatest absolute value of signed
        //  The bounds of signed lie within [-ub(absolute), ub(absolute)] at the same time.
        if absolute_upper != greatest_absolute {
            log::error!(
                "The upper bound of {:?} is {absolute_upper} while the greatest absolute value of {:?} is {greatest_absolute}",
                self.absolute,
                self.signed
            );
            return false;
        }

        // 3. Assert that the bound of signed nearest to zero is at least the lower bound of
        //    absolute in magnitude when the sign of signed is fixed
        //  When signed can be 0, the propagator does not remove the values nearest to zero.
        if signed_upper <= 0 && -signed_upper < absolute_lower {
            log::error!(
                "The upper bound of {:?} could be lowered to {} by the lower bound of {:?}",
                self.signed,
                -absolute_lower,
                self.absolute
            );
            return false;
        }
        if signed_lower >= 0 && signed_lower < absolute_lower {
            log::error!(
                "The lower bound of {:?} could be raised to {absolute_lower} by the lower bound of {:?}",
                self.signed,
                self.absolute
            );
            return false;
        }

        true
    }
}
