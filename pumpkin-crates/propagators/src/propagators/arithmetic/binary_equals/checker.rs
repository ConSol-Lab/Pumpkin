use pumpkin_checking::AtomicConstraint;
use pumpkin_checking::CheckerVariable;
use pumpkin_checking::InferenceChecker;
use pumpkin_checking::IntExt;
use pumpkin_core::checkers::support::Support;
use pumpkin_core::checkers::support::SupportGenerator;
use pumpkin_core::checkers::support::SupportsValue;
use pumpkin_core::checkers::support::UnsupportedValue;
use pumpkin_core::propagation::Domains;
use pumpkin_core::propagation::LocalId;
use pumpkin_core::variables::IntegerVariable;

#[derive(Clone, Debug)]
pub struct BinaryEqualsChecker<Lhs, Rhs> {
    pub lhs: Lhs,
    pub rhs: Rhs,
}

impl<Lhs, Rhs, Atomic> InferenceChecker<Atomic> for BinaryEqualsChecker<Lhs, Rhs>
where
    Atomic: AtomicConstraint,
    Lhs: CheckerVariable<Atomic>,
    Rhs: CheckerVariable<Atomic>,
{
    fn check(
        &self,
        mut state: pumpkin_checking::VariableState<Atomic>,
        _: &[Atomic],
        _: Option<&Atomic>,
    ) -> bool {
        // We apply the domain of variable 2 to variable 1. If the state remains consistent, then
        // the step is unsound!
        let mut consistent = true;

        if let IntExt::Int(value) = self.rhs.induced_upper_bound(&state) {
            let atomic = self.lhs.atomic_less_than(value);
            consistent &= state.apply(&atomic);
        }

        if let IntExt::Int(value) = self.rhs.induced_lower_bound(&state) {
            let atomic = self.lhs.atomic_greater_than(value);
            consistent &= state.apply(&atomic);
        }

        for value in self.rhs.induced_holes(&state).collect::<Vec<_>>() {
            let atomic = self.lhs.atomic_not_equal(value);
            consistent &= state.apply(&atomic);
        }

        !consistent
    }
}

impl<Lhs, Rhs> SupportGenerator for BinaryEqualsChecker<Lhs, Rhs>
where
    Lhs: IntegerVariable + SupportsValue,
    Rhs: IntegerVariable + SupportsValue,
{
    type Value = i32;

    fn support(
        &mut self,
        support: &mut Support<Self::Value>,
        local_id: LocalId,
        value: UnsupportedValue,
        _: &Domains<'_>,
    ) {
        let value = match local_id {
            super::ID_LHS => self.lhs.unpack(value),
            super::ID_RHS => self.rhs.unpack(value),
            _ => unreachable!(),
        };

        self.lhs.assign(value, support);
        self.rhs.assign(value, support);
    }

    fn is_solution(&self, support: &Support<Self::Value>) -> bool {
        self.lhs.support_value(support) == self.rhs.support_value(support)
    }
}
