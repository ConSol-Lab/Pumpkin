use std::fmt::Debug;

use pumpkin_checking::AtomicConstraint;
use pumpkin_checking::InferenceChecker;
use pumpkin_checking::VariableState;

use crate::checkers::ConsistencyChecker;
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

impl ConsistencyChecker for NogoodChecker<Predicate> {
    fn check_consistency(&mut self, _: &Scope, domains: Domains<'_>) -> bool {
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
