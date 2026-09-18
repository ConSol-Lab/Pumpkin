use std::collections::BTreeSet;

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

impl<Lhs, Rhs> RetentionChecker for BinaryEqualsChecker<Lhs, Rhs>
where
    Lhs: IntegerVariable + 'static,
    Rhs: IntegerVariable + 'static,
{
    fn check_retention(&mut self, _: &Scope, domains: Domains<'_>) -> bool {
        // 1. Assert that the bounds are equal
        let lower = domains.lower_bound(&self.lhs);
        let upper = domains.upper_bound(&self.lhs);
        let same_bounds =
            lower == domains.lower_bound(&self.rhs) && upper == domains.upper_bound(&self.rhs);
        // 2. Assert that the holes within the bounds are equal
        //  A domain may record holes outside its bounds, so only those within the shared bounds
        //  are compared.
        let are_equal = same_bounds
            && holes_within(&domains, &self.lhs, lower, upper)
                == holes_within(&domains, &self.rhs, lower, upper);

        if !are_equal {
            log::error!(
                "The domains of {:?} and {:?} differ although the two are equal: {:?} and {:?}",
                self.lhs,
                self.rhs,
                domains.iterate_domain(&self.lhs).collect::<Vec<_>>(),
                domains.iterate_domain(&self.rhs).collect::<Vec<_>>()
            );
        }

        are_equal
    }
}

fn holes_within<Var: IntegerVariable>(
    domains: &Domains<'_>,
    variable: &Var,
    lower: i32,
    upper: i32,
) -> BTreeSet<i32> {
    domains
        .get_holes(variable)
        .filter(|&value| lower <= value && value <= upper)
        .collect()
}

#[cfg(test)]
mod retention_tests {
    use pumpkin_core::predicate;
    use pumpkin_core::state::State;

    use super::*;
    use crate::arithmetic::BinaryEqualsPropagatorArgs;

    #[test]
    fn retention_fails_when_the_bounds_differ() {
        let mut state = State::default();
        let a = state.new_interval_variable(0, 5, None);
        let b = state.new_interval_variable(3, 8, None);

        let mut checker = BinaryEqualsChecker { lhs: a, rhs: b };
        let scope = Scope::from_variables([a, b].iter());

        assert!(!checker.check_retention(&scope, state.get_domains()));
    }

    #[test]
    fn retention_fails_when_a_hole_is_not_shared() {
        let mut state = State::default();
        let a = state.new_interval_variable(0, 5, None);
        let b = state.new_interval_variable(0, 5, None);
        let _ = state.post(predicate![a != 2]).unwrap();

        let mut checker = BinaryEqualsChecker { lhs: a, rhs: b };
        let scope = Scope::from_variables([a, b].iter());

        assert!(!checker.check_retention(&scope, state.get_domains()));
    }

    #[test]
    fn retention_holds_when_the_domains_agree_but_the_recorded_holes_differ() {
        let mut state = State::default();
        let a = state.new_interval_variable(1, 3, None);
        let b = state.new_interval_variable(3, 3, None);
        let _ = state.post(predicate![a != 1]).unwrap();
        let _ = state.post(predicate![a != 2]).unwrap();

        let mut checker = BinaryEqualsChecker { lhs: a, rhs: b };
        let scope = Scope::from_variables([a, b].iter());

        assert!(checker.check_retention(&scope, state.get_domains()));
    }

    #[test]
    fn retention_holds_at_the_fixpoint_of_the_propagator() {
        let mut state = State::default();
        let a = state.new_interval_variable(0, 5, None);
        let b = state.new_interval_variable(3, 8, None);
        let constraint_tag = state.new_constraint_tag();

        let _ = state.add_propagator(BinaryEqualsPropagatorArgs {
            a,
            b,
            constraint_tag,
        });
        state.propagate_to_fixed_point().expect("no empty domains");

        let mut checker = BinaryEqualsChecker { lhs: a, rhs: b };
        let scope = Scope::from_variables([a, b].iter());

        assert!(checker.check_retention(&scope, state.get_domains()));
    }
}
