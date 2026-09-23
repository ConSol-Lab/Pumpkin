use super::IntegerDivisionChecker;
use crate::AtomicConstraint;
use crate::CheckerVariable;
use crate::InferenceChecker;
use crate::IntExt;
use crate::VariableState;

impl<VA, VB, VC, Atomic> InferenceChecker<Atomic> for IntegerDivisionChecker<VA, VB, VC>
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
        _consequent: Option<&Atomic>,
    ) -> bool {
        // We apply interval arithmetic to determine that the computed interval `a div b`
        // does not intersect with the domain of `c`.
        //
        // See https://en.wikipedia.org/wiki/Interval_arithmetic#Interval_operators.

        let x1 = self.numerator.induced_lower_bound(&state);
        let x2 = self.numerator.induced_upper_bound(&state);
        let y1 = self.denominator.induced_lower_bound(&state);
        let y2 = self.denominator.induced_upper_bound(&state);

        assert!(
            y2 < 0 || y1 > 0,
            "Currentl, the checker does not contain inferences where the denominator spans 0"
        );

        let computed_c_lower: IntExt = *[
            x1.div_ceil(y1),
            x1.div_ceil(y2),
            x2.div_ceil(y1),
            x2.div_ceil(y2),
        ]
        .iter()
        .flatten()
        .min()
        .expect("Expected at least one element to be defined");

        let computed_c_upper: IntExt = *[
            x1.div_floor(y1),
            x1.div_floor(y2),
            x2.div_floor(y1),
            x2.div_floor(y2),
        ]
        .iter()
        .flatten()
        .max()
        .expect("Expected at least one element to be defined");

        let c_lower = self.rhs.induced_lower_bound(&state);
        let c_upper = self.rhs.induced_upper_bound(&state);

        computed_c_upper < c_lower || computed_c_lower > c_upper
    }
}
