use std::fmt::Debug;

use pumpkin_checking::AtomicConstraint;
use pumpkin_checking::InferenceChecker;
use pumpkin_checking::VariableState;

use crate::checkers::RetentionChecker;
use crate::checkers::Scope;
use crate::containers::HashSet;
use crate::predicates::Predicate;
use crate::predicates::PredicateType;
use crate::propagation::Domains;
use crate::propagation::ReadDomains;

#[derive(Debug, Clone)]
pub struct NogoodChecker<Atomic> {
    pub nogood: Box<[Atomic]>,
}

impl<Atomic> InferenceChecker<Atomic> for NogoodChecker<Atomic>
where
    Atomic: AtomicConstraint + Clone + Debug,
{
    fn check(&self, state: VariableState<Atomic>, _: &[Atomic], _: Option<&Atomic>) -> bool {
        self.nogood.iter().all(|atomic| state.is_true(atomic))
    }
}

impl RetentionChecker for NogoodChecker<Predicate> {
    fn check_retention(&mut self, _: &Scope, domains: Domains<'_>) -> bool {
        // For unit propagation, the state is consistent if:
        // - at least two predicates are unassigned
        // - or otherwise, at least one predicate is assigned

        let untrue_predicate_count = self
            .nogood
            .iter()
            .filter(|&&predicate| domains.evaluate_predicate(predicate) != Some(true))
            .count();

        // If at least two predicates are not true, or any predicate is false, then the domains are
        // unit-propagation consistent.
        let is_consistent = untrue_predicate_count >= 2
            || self
                .nogood
                .iter()
                .any(|&predicate| domains.evaluate_predicate(predicate) == Some(false));

        if !is_consistent {
            log::error!(
                "The nogood {:?} is not unit-propagation consistent; truth values: {:?}",
                self.nogood,
                self.nogood
                    .iter()
                    .map(|&predicate| (predicate, domains.evaluate_predicate(predicate)))
                    .collect::<Vec<_>>()
            );
        }

        is_consistent
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::conjunction;
    use crate::propagation::LocalId;
    use crate::state::State;

    #[test]
    fn a_nogood_with_multiple_untrue_predicates_is_consistent() {
        let mut state = State::default();

        let x = state.new_interval_variable(1, 5, Some("x".into()));
        let y = state.new_interval_variable(1, 5, Some("y".into()));

        let mut checker = NogoodChecker {
            nogood: conjunction!([x >= 4] & [y <= 2]).into(),
        };

        let scope = Scope::from_iter([(LocalId::from(0), x), (LocalId::from(1), y)]);
        assert!(checker.check_retention(&scope, state.get_domains()));
    }

    #[test]
    fn a_nogood_with_one_untrue_predicates_and_no_false_predicates_is_inconsistent() {
        let mut state = State::default();

        let x = state.new_interval_variable(1, 5, Some("x".into()));
        let y = state.new_interval_variable(1, 5, Some("y".into()));

        let mut checker = NogoodChecker {
            nogood: conjunction!([x >= 4] & [y <= 5]).into(),
        };

        let scope = Scope::from_iter([(LocalId::from(0), x), (LocalId::from(1), y)]);
        assert!(!checker.check_retention(&scope, state.get_domains()));
    }

    #[test]
    fn a_nogood_with_any_false_predicates_is_consistent() {
        let mut state = State::default();

        let x = state.new_interval_variable(1, 3, Some("x".into()));
        let y = state.new_interval_variable(1, 5, Some("y".into()));

        let mut checker = NogoodChecker {
            nogood: conjunction!([x >= 4] & [y <= 2]).into(),
        };

        let scope = Scope::from_iter([(LocalId::from(0), x), (LocalId::from(1), y)]);
        assert!(checker.check_retention(&scope, state.get_domains()));
    }
}

/// The retention checker for extended nogood propagation: when the atomic constraints over all
/// but one variable hold, that variable has no value left that satisfies its atomic constraints.
#[derive(Debug, Clone)]
pub struct ExtendedNogoodChecker {
    pub nogood: Box<[Predicate]>,
}

impl RetentionChecker for ExtendedNogoodChecker {
    fn check_retention(&mut self, _: &Scope, domains: Domains<'_>) -> bool {
        // 1. Determine the variables with a predicate which is not true; if there are none then the
        //    nogood is conflicting
        let free_domains = self
            .nogood
            .iter()
            .filter(|&&predicate| domains.evaluate_predicate(predicate) != Some(true))
            .map(|predicate| predicate.get_domain())
            .collect::<Vec<_>>();
        if free_domains.is_empty() {
            log::error!(
                "The nogood {:?} holds; it should have been reported as a conflict",
                self.nogood
            );
            return false;
        }

        // 2. If predicates over at least two variables are not true then nothing can be propagated
        let free_domain = free_domains[0];
        if free_domains.iter().any(|&domain| domain != free_domain) {
            return true;
        }

        // 3. Determine the values of the remaining variable which satisfy all of its predicates
        let mut lower = domains.lower_bound(&free_domain);
        let mut upper = domains.upper_bound(&free_domain);
        let mut excluded: HashSet<i32> = domains.get_holes(&free_domain).collect();
        for predicate in self
            .nogood
            .iter()
            .filter(|predicate| predicate.get_domain() == free_domain)
        {
            let value = predicate.get_right_hand_side();
            match predicate.get_predicate_type() {
                PredicateType::LowerBound => lower = lower.max(value),
                PredicateType::UpperBound => upper = upper.min(value),
                PredicateType::NotEqual => {
                    let _ = excluded.insert(value);
                }
                PredicateType::Equal => {
                    lower = lower.max(value);
                    upper = upper.min(value);
                }
            }
        }

        // 4. Assert that none of these values remain in the domain
        let num_values = (i64::from(upper) - i64::from(lower) + 1).max(0);
        let num_excluded = excluded
            .iter()
            .filter(|&&value| lower <= value && value <= upper)
            .count() as i64;
        let no_value_allowed = num_excluded == num_values;

        if !no_value_allowed {
            log::error!(
                "The values of {free_domain} in [{lower}, {upper}] could be removed by the nogood {:?}",
                self.nogood
            );
        }

        no_value_allowed
    }
}

#[cfg(test)]
mod extended_tests {
    use super::*;
    use crate::conjunction;
    use crate::predicate;
    use crate::propagation::LocalId;
    use crate::state::State;

    #[test]
    fn a_free_variable_with_allowed_values_is_not_consistent_under_extended_propagation() {
        let mut state = State::default();
        let x = state.new_interval_variable(0, 10, Some("x".into()));
        let y = state.new_interval_variable(1, 1, Some("y".into()));
        let nogood: Box<[Predicate]> = conjunction!([x >= 3] & [x <= 5] & [y == 1]).into();
        let scope = Scope::from_iter([(LocalId::from(0), x), (LocalId::from(1), y)]);

        let mut extended = ExtendedNogoodChecker {
            nogood: nogood.clone(),
        };
        assert!(!extended.check_retention(&scope, state.get_domains()));

        // Unit propagation cannot fire with two atomic constraints over `x` unassigned.
        let mut unit = NogoodChecker { nogood };
        assert!(unit.check_retention(&scope, state.get_domains()));
    }

    #[test]
    fn a_free_variable_without_allowed_values_is_consistent() {
        let mut state = State::default();
        let x = state.new_interval_variable(0, 10, Some("x".into()));
        let y = state.new_interval_variable(1, 1, Some("y".into()));
        for value in 3..=5 {
            let _ = state.post(predicate![x != value]).unwrap();
        }

        let mut checker = ExtendedNogoodChecker {
            nogood: conjunction!([x >= 3] & [x <= 5] & [y == 1]).into(),
        };
        let scope = Scope::from_iter([(LocalId::from(0), x), (LocalId::from(1), y)]);

        assert!(checker.check_retention(&scope, state.get_domains()));
    }

    #[test]
    fn two_free_variables_are_consistent() {
        let mut state = State::default();
        let x = state.new_interval_variable(0, 10, Some("x".into()));
        let y = state.new_interval_variable(0, 5, Some("y".into()));

        let mut checker = ExtendedNogoodChecker {
            nogood: conjunction!([x >= 3] & [x <= 5] & [y == 1]).into(),
        };
        let scope = Scope::from_iter([(LocalId::from(0), x), (LocalId::from(1), y)]);

        assert!(checker.check_retention(&scope, state.get_domains()));
    }

    #[test]
    fn a_nogood_that_holds_is_not_consistent() {
        let mut state = State::default();
        let x = state.new_interval_variable(4, 4, Some("x".into()));
        let y = state.new_interval_variable(1, 1, Some("y".into()));

        let mut checker = ExtendedNogoodChecker {
            nogood: conjunction!([x >= 3] & [x <= 5] & [y == 1]).into(),
        };
        let scope = Scope::from_iter([(LocalId::from(0), x), (LocalId::from(1), y)]);

        assert!(!checker.check_retention(&scope, state.get_domains()));
    }
}
