use std::fmt::Debug;

use pumpkin_checking::AtomicConstraint;
use pumpkin_checking::InferenceChecker;
use pumpkin_checking::VariableState;

use crate::checkers::RetentionChecker;
use crate::checkers::Scope;
use crate::predicates::Predicate;
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

        if untrue_predicate_count >= 2 {
            // If at least two predicates are not true, then the domain is
            // unit-propagation consistent.
            return true;
        }

        // At least one predicate must be false for the domain to be unit-propagation consistent.
        self.nogood
            .iter()
            .any(|&predicate| domains.evaluate_predicate(predicate) == Some(false))
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
