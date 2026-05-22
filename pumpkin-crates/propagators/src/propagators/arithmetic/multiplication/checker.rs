use pumpkin_checking::AtomicConstraint;
use pumpkin_checking::CheckerVariable;
use pumpkin_checking::InferenceChecker;
use pumpkin_core::checkers::support::Support;
use pumpkin_core::checkers::support::SupportGenerator;
use pumpkin_core::checkers::support::SupportsValue;
use pumpkin_core::checkers::support::UnsupportedValue;
use pumpkin_core::propagation::Domains;
use pumpkin_core::propagation::LocalId;
use pumpkin_core::propagation::ReadDomains;
use pumpkin_core::variables::IntegerVariable;

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

impl<VA, VB, VC> SupportGenerator for IntegerMultiplicationChecker<VA, VB, VC>
where
    VA: IntegerVariable + SupportsValue<f32>,
    VB: IntegerVariable + SupportsValue<f32>,
    VC: IntegerVariable + SupportsValue<f32>,
{
    type Value = f32;

    fn support(
        &mut self,
        support: &mut Support<Self::Value>,
        local_id: LocalId,
        unsupported_value: UnsupportedValue,
        domains: &Domains<'_>,
    ) {
        let a_min = domains.lower_bound(&self.a) as f32;
        let a_max = domains.upper_bound(&self.a) as f32;
        let b_min = domains.lower_bound(&self.b) as f32;
        let b_max = domains.upper_bound(&self.b) as f32;
        let c_min = domains.lower_bound(&self.c) as f32;
        let c_max = domains.upper_bound(&self.c) as f32;

        let (value_a, value_b, value_c) = match local_id {
            super::ID_A => {
                let value_a = self.a.unpack(unsupported_value) as f32;

                let Some((value_b, value_c)) = [b_min, b_max].into_iter().find_map(|value_b| {
                    let value_c = value_a * value_b;
                    if c_min <= value_c && value_c <= c_max {
                        Some((value_b, value_c))
                    } else {
                        None
                    }
                }) else {
                    return;
                };

                (value_a, value_b, value_c)
            }
            super::ID_B => {
                let value_b = self.b.unpack(unsupported_value) as f32;

                let Some((value_a, value_c)) = [a_min, a_max].into_iter().find_map(|value_a| {
                    let value_c = value_a * value_b;
                    if c_min <= value_c && value_c <= c_max {
                        Some((value_a, value_c))
                    } else {
                        None
                    }
                }) else {
                    return;
                };

                (value_a, value_b, value_c)
            }
            super::ID_C => {
                let value_c = self.c.unpack(unsupported_value) as f32;

                let Some(values) = [a_min, a_max].into_iter().find_map(|value_a| {
                    let value_b = value_c / value_a;

                    if b_min <= value_b && value_b <= b_max {
                        Some((value_a, value_b, value_c))
                    } else {
                        None
                    }
                }) else {
                    return;
                };

                values
            }

            _ => unreachable!(),
        };

        self.a.assign(value_a, support);
        self.b.assign(value_b, support);
        self.c.assign(value_c, support);
    }

    fn is_solution(&self, support: &Support<Self::Value>) -> bool {
        self.a.support_value(support) * self.b.support_value(support)
            == self.c.support_value(support)
    }
}
