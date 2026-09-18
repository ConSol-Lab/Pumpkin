use pumpkin_checking::AtomicConstraint;
use pumpkin_checking::CheckerVariable;
use pumpkin_checking::InferenceChecker;
use pumpkin_checking::IntExt;
use pumpkin_core::checkers::RetentionChecker;
use pumpkin_core::checkers::Scope;
use pumpkin_core::propagation::Domains;
use pumpkin_core::propagation::ReadDomains;
use pumpkin_core::variables::IntegerVariable;

#[derive(Clone, Debug)]
pub struct MaximumChecker<ElementVar, Rhs> {
    pub array: Box<[ElementVar]>,
    pub rhs: Rhs,
}

impl<ElementVar, Rhs, Atomic> InferenceChecker<Atomic> for MaximumChecker<ElementVar, Rhs>
where
    Atomic: AtomicConstraint,
    ElementVar: CheckerVariable<Atomic>,
    Rhs: CheckerVariable<Atomic>,
{
    fn check(
        &self,
        state: pumpkin_checking::VariableState<Atomic>,
        _: &[Atomic],
        _: Option<&Atomic>,
    ) -> bool {
        let lowest_maximum = self
            .array
            .iter()
            .map(|element| element.induced_lower_bound(&state))
            .max()
            .unwrap_or(IntExt::NegativeInf);
        let highest_maximum = self
            .array
            .iter()
            .map(|element| element.induced_upper_bound(&state))
            .max()
            .unwrap_or(IntExt::PositiveInf);

        // If the intersection between the domain of `rhs` and `[lowest_maximum,
        // highest_maximum]` is empty, there is a conflict.

        lowest_maximum > self.rhs.induced_upper_bound(&state)
            || highest_maximum < self.rhs.induced_lower_bound(&state)
    }
}

impl<ElementVar, Rhs> RetentionChecker for MaximumChecker<ElementVar, Rhs>
where
    ElementVar: IntegerVariable + 'static,
    Rhs: IntegerVariable + 'static,
{
    fn check_retention(&mut self, _: &Scope, domains: Domains<'_>) -> bool {
        let rhs_lower = domains.lower_bound(&self.rhs);
        let rhs_upper = domains.upper_bound(&self.rhs);

        let Some(greatest_lower) = self
            .array
            .iter()
            .map(|element| domains.lower_bound(element))
            .max()
        else {
            return false;
        };
        let greatest_upper = self
            .array
            .iter()
            .map(|element| domains.upper_bound(element))
            .max()
            .expect("the array has an element");

        if rhs_lower < greatest_lower {
            log::error!(
                "The lower bound of {:?} could be raised to {greatest_lower} by the maximum of {:?}",
                self.rhs,
                self.array
            );
            return false;
        }

        if rhs_upper > greatest_upper {
            log::error!(
                "The upper bound of {:?} could be lowered to {greatest_upper} by the maximum of {:?}",
                self.rhs,
                self.array
            );
            return false;
        }

        for element in self.array.iter() {
            if domains.upper_bound(element) > rhs_upper {
                log::error!(
                    "The upper bound of {element:?} could be lowered to {rhs_upper}, the upper bound of the maximum {:?}",
                    self.rhs
                );
                return false;
            }
        }

        // When a single element can reach the lower bound of the maximum, it has to attain it.
        // Elements are counted by position, as the propagator does.
        let mut candidates = self
            .array
            .iter()
            .filter(|&element| domains.upper_bound(element) >= rhs_lower);
        if let (Some(candidate), None) = (candidates.next(), candidates.next())
            && domains.lower_bound(candidate) < rhs_lower
        {
            log::error!(
                "The lower bound of {candidate:?} could be raised to {rhs_lower}: it is the only element that can attain the maximum {:?}",
                self.rhs
            );
            return false;
        }

        true
    }
}

#[cfg(test)]
mod tests {
    use pumpkin_core::state::State;

    use super::*;
    use crate::arithmetic::MaximumArgs;

    #[test]
    fn retention_fails_when_the_lower_bound_of_the_maximum_is_below_the_greatest_lower_bound() {
        let mut state = State::default();
        let a = state.new_interval_variable(3, 10, None);
        let b = state.new_interval_variable(0, 5, None);
        let rhs = state.new_interval_variable(0, 10, None);

        let mut checker = MaximumChecker {
            array: [a, b].into(),
            rhs,
        };
        let scope = Scope::from_variables([a, b, rhs].iter());

        assert!(!checker.check_retention(&scope, state.get_domains()));
    }

    #[test]
    fn retention_fails_when_an_element_exceeds_the_upper_bound_of_the_maximum() {
        let mut state = State::default();
        let a = state.new_interval_variable(0, 10, None);
        let b = state.new_interval_variable(0, 5, None);
        let rhs = state.new_interval_variable(0, 8, None);

        let mut checker = MaximumChecker {
            array: [a, b].into(),
            rhs,
        };
        let scope = Scope::from_variables([a, b, rhs].iter());

        assert!(!checker.check_retention(&scope, state.get_domains()));
    }

    #[test]
    fn retention_fails_when_the_sole_candidate_does_not_attain_the_lower_bound_of_the_maximum() {
        let mut state = State::default();
        let a = state.new_interval_variable(0, 8, None);
        let b = state.new_interval_variable(0, 3, None);
        let rhs = state.new_interval_variable(5, 8, None);

        let mut checker = MaximumChecker {
            array: [a, b].into(),
            rhs,
        };
        let scope = Scope::from_variables([a, b, rhs].iter());

        assert!(!checker.check_retention(&scope, state.get_domains()));
    }

    #[test]
    fn retention_holds_at_the_fixpoint_of_the_propagator() {
        let mut state = State::default();
        let a = state.new_interval_variable(0, 10, None);
        let b = state.new_interval_variable(0, 3, None);
        let rhs = state.new_interval_variable(5, 8, None);
        let constraint_tag = state.new_constraint_tag();

        let _ = state.add_propagator(MaximumArgs {
            array: [a, b].into(),
            rhs,
            constraint_tag,
        });
        state.propagate_to_fixed_point().expect("no empty domains");

        let mut checker = MaximumChecker {
            array: [a, b].into(),
            rhs,
        };
        let scope = Scope::from_variables([a, b, rhs].iter());

        assert!(checker.check_retention(&scope, state.get_domains()));
    }
}
