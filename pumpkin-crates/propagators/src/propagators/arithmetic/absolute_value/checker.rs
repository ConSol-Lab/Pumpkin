use pumpkin_checking::AtomicConstraint;
use pumpkin_checking::CheckerVariable;
use pumpkin_checking::ConflictChecker;
use pumpkin_checking::IntExt;

#[derive(Clone, Debug)]
pub struct AbsoluteValueChecker<VA, VB> {
    pub(super) signed: VA,
    pub(super) absolute: VB,
}

impl<VA, VB, Atomic> ConflictChecker<Atomic> for AbsoluteValueChecker<VA, VB>
where
    VA: CheckerVariable<Atomic>,
    VB: CheckerVariable<Atomic>,
    Atomic: AtomicConstraint,
{
    fn check(
        &self,
        state: pumpkin_checking::VariableState<Atomic>,
        _: &[Atomic],
        _: Option<&Atomic>,
    ) -> bool {
        let signed_lower = self.signed.induced_lower_bound(&state);
        let signed_upper = self.signed.induced_upper_bound(&state);
        let absolute_lower = self.absolute.induced_lower_bound(&state);
        let absolute_upper = self.absolute.induced_upper_bound(&state);

        if absolute_lower < 0 {
            // The absolute value cannot have negative values.
            return true;
        }

        // Now we compute the interval for |signed| based on the domain of signed.
        let (computed_signed_lower, computed_signed_upper) = if signed_lower >= 0 {
            (signed_lower, signed_upper)
        } else if signed_upper <= 0 {
            (-signed_upper, -signed_lower)
        } else if signed_lower < 0 && 0_i32 < signed_upper {
            (IntExt::Int(0), std::cmp::max(-signed_lower, signed_upper))
        } else {
            unreachable!()
        };

        // The intervals should not match, otherwise there is no conflict.
        computed_signed_lower != absolute_lower || computed_signed_upper != absolute_upper
    }
}
