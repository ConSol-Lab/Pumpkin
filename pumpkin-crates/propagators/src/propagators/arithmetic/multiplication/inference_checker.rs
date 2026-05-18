use pumpkin_checking::AtomicConstraint;
use pumpkin_checking::CheckerVariable;
use pumpkin_checking::InferenceChecker;

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

        computed_c_upper < c_lower || computed_c_lower > c_upper
    }
}
